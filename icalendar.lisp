
(cl:defpackage :soiree-icalendar
  (:use :common-lisp :parser-combinators :soiree :soiree-parse)
  (:shadow #:class #:method))

(cl:in-package :soiree-icalendar)

(defvar *ical-namespace* "urn:ietf:params:xml:ns:icalendar-2.0")

(defvar *ical-rng-pathname*
  (merge-pathnames #p"icalendar-2.0.rnc" soiree-config:*base-directory*))

(defvar *ical-rng-schema* (cxml-rng:parse-compact *ical-rng-pathname*))

;; Value type utility functions
(defun digits-to-number (chars)
  (reduce (lambda (acc dig) (+ dig (* 10 acc))) chars :initial-value 0))

(defun decimal? ()
  (named-seq?
   (<- int-part (int?))
   (<- frac-part
       (opt?
        (named-seq?
         #\.
         (<- frac-part (many? (hook? #'digit-char-p (digit?))))
         frac-part)))
   (+ int-part (or (when frac-part
                     (* (signum int-part)
                        (/ (digits-to-number frac-part)
                           (float (expt 10 (length frac-part))))))
                   0))))
(defun date? ()
  (hook? (lambda (x)
           (destructuring-bind (year-digits month-digits day-digits) x
             (let ((year (digits-to-number year-digits))
                   (month (+ (* 10 (first month-digits)) (second month-digits)))
                   (day (+ (* 10 (first day-digits)) (second month-digits))))
               (list year month day))))
         (seq-list? (times? (hook? #'digit-char-p (digit?)) 4)
                    (times? (hook? #'digit-char-p (digit?)) 2)
                    (times? (hook? #'digit-char-p (digit?)) 2))))

(defun time? ()
  (hook? (lambda (x)
           (destructuring-bind (hour-digits minute-digits second-digits time-utc) x
             (let ((hour (digits-to-number hour-digits))
                   (minute (+ (* 10 (first minute-digits)) (second minute-digits)))
                   (second (+ (* 10 (first second-digits)) (second minute-digits))))
               (list hour minute second time-utc))))
         (seq-list? (times? (hook? #'digit-char-p (digit?)) 2)
                    (times? (hook? #'digit-char-p (digit?)) 2)
                    (times? (hook? #'digit-char-p (digit?)) 2)
                    (opt? #\Z))))

(defun convert-icalendar-date-to-xcal (string)
  (parse-string*
   (date?)
   string))

(defun convert-icalendar-date-time-to-xcal (string)
  (parse-string*
   (named-seq?
    (<- date (date?))
    #\T
    (<- time (time?))
    (list date time))
   string))

;;; Parameters

(defmacro def-generic-parameter (function-name param-name &key (node-type "text"))
  `(defun ,function-name (params)
     (when-let (param-values
                (mapcan #'second (keep ,param-name params :test #'string-equal :key #'car)))
       (reduce (lambda (parent param-value)
                 (stp:append-child parent (make-text-node ,node-type param-value)))
               param-values
               :initial-value (stp:make-element ,param-name *ical-namespace*)))))


;; 3.2.1 Alternate Text Representation
(def-generic-parameter altrepparam "altrep" :node-type "uri")

;; 3.2.7 Inline Encoding
(defun encodingparam (params)
  (when-let (encodings
             (mapcan #'second (keep "encoding" params :test #'string-equal :key #'car)))
    (reduce (lambda (parent encoding)
              (unless (member encoding '("8BIT" "BASE64") :test #'string=)
                (warn "Unknown encoding parameter: ~A" encoding))
              (stp:append-child parent (make-text-node "text" encoding)))
            encodings
            :initial-value (stp:make-element "encoding" *ical-namespace*))))

;; 3.2.8 Format Type
(defun fmttypeparam (params)
  (when-let (fmttypes
             (mapcan #'second (keep "fmttype" params :test #'string-equal :key #'car)))
    (reduce (lambda (parent fmttype)
              (stp:append-child parent (make-text-node "text" fmttype)))
            fmttypes
            :initial-value (stp:make-element "fmttype" *ical-namespace*))))

;; 3.2.10 Language
(defun languageparam (params)
  (when-let (language
             (caadar (keep "language" params :test #'string-equal :key #'car)))
    (stp:append-child (stp:make-element "language" *ical-namespace*)
                      (make-text-node "text" language))))

;;; Properties

;; Property utility functions

(defun add-params (param-list params)
  (remove nil (mapcar (lambda (x) (funcall x params)) param-list) :test 'eq))

(defun extract-parameters (params functions)
  (let ((param-element (stp:make-element "parameters" *ical-namespace*)))
    (let ((param-children (add-params functions params)))
      (reduce #'stp:append-child param-children :initial-value param-element))))

(defmacro def-generic-property (property-name element-name
                                parameter-functions value-node-type
                                &key multiple-values)
  `(defun ,property-name (result)
     (destructuring-bind (group name params value) result
       (declare (ignore group name))
       (let ((node (stp:make-element ,element-name *ical-namespace*))
             (param-element (extract-parameters
                             params
                             ,parameter-functions)))
         (when (plusp (stp:number-of-children param-element))
           (stp:append-child node param-element))
         ,(if multiple-values
              `(reduce #'stp:append-child
                       (apply #'make-text-node-list ,value-node-type
                              (split-string value))
                       :initial-value node)
              `(stp:append-child
                node
                (make-text-node ,value-node-type value)))))))

;; 3.8.1 Descriptive Component Properties

;; 3.8.1.1 Attachment
(defun attach (result)
  (destructuring-bind
      (group name params value) result
    (declare (ignore group name))
    (let ((node (cxml-stp:make-element "attach" *ical-namespace*))
          (param-element
            (extract-parameters params
                                (list #'fmttypeparam #'encodingparam))))
      (when (plusp (cxml-stp:number-of-children param-element))
        (cxml-stp:append-child node param-element))
      (let ((encoding
              (or (caadar (keep "encoding" params :test #'string-equal :key #'car)))))
        (cxml-stp:append-child
         node
         (cond ((string= encoding "BINARY")
                (make-text-node "binary" value))
               (encoding
                (make-text-node "binary" (base64:base64-string-to-string value)))
               (t
                (make-text-node "uri" value))))))))

;; 3.8.1.2 Categories
(def-generic-property categories "categories"
  (list #'languageparam) "text"
  :multiple-values t)

;; 3.8.1.3 Classification
(def-generic-property class "class" nil "text")

;; 3.8.1.4 Comment
(def-generic-property comment "comment"
  (list #'altrepparam #'languageparam) "text")

;; 3.8.1.5 Description
(def-generic-property description "description"
  (list #'altrepparam #'languageparam) "text")

;; 3.8.1.6 Geographic Position
(defun geo (result)
  (destructuring-bind
      (group name params value)
      result
    (declare (ignore group name params))
    (let ((node (cxml-stp:make-element "geo" *ical-namespace*)))
      (destructuring-bind (lat long)
          (parse-string*
           (named-seq?
            (<- lat (decimal?))
            #\;
            (<- long (decimal?))
            (list lat long))
           value)
        (reduce #'cxml-stp:append-child 
                (list (make-text-node "latitude" (prin1-to-string lat))
                      (make-text-node "longitude" (prin1-to-string long)))
                :initial-value node)))))

;; 3.8.1.7 Location
(def-generic-property location "location"
  (list #'altrepparam #'languageparam) "text")


;; 3.8.7 Change Management Component Properties

(defun make-date-node (element-tag string)
  (stp:append-child
   (stp:make-element (string-downcase element-tag) *ical-namespace*)
   (stp:append-child
    (stp:make-element "date" *ical-namespace*)
    (destructuring-bind (year month day)
        (convert-icalendar-date-to-xcal string)
      (stp:make-text (format nil "~2,'0D-~2,'0D-~2,'0D" year month day))))))

(defun make-date-time-node (element-tag string)
  (stp:append-child
   (stp:make-element (string-downcase element-tag) *ical-namespace*)
   (stp:append-child
    (stp:make-element "date-time" *ical-namespace*)
    (destructuring-bind ((year month day)
                         (hours minute second utc))
        (convert-icalendar-date-time-to-xcal string)
      (stp:make-text (format nil "~2,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D"
                             year month day hours minute second))))))

;; FIXME! Add tz support here!
(defun date-time-node (result)
  (destructuring-bind (group name params value) result
    (declare (ignore group params))
    (make-date-time-node name value)))

;; FIXME! Add tz support here!
(defun date-time-or-date-node (result)
  (destructuring-bind (group name params value) result
    (declare (ignore group))
    (let ((value-type
            (or (caadar (keep "value" params :test #'string-equal :key #'car)))))
      (cond ((string-equal value-type "DATE")
             (make-date-node name value))
            (t
             (make-date-time-node name value))))))

(defun dtstamp (result) (date-time-node result))
(defun dtstart (result) (date-time-or-date-node result))
(defun dtend (result) (date-time-or-date-node result))

(defun make-cal-address-node (element-tag string)
  (stp:append-child
   (stp:make-element (string-downcase element-tag) *ical-namespace*)
   (stp:append-child
    (stp:make-element "cal-address" *ical-namespace*)
    (stp:make-text string))))

(defun cal-address-node (result)
  (destructuring-bind (group name params value) result
    (declare (ignore group params))
    (make-cal-address-node (string-downcase name) value)))

(defun attendee (result) (cal-address-node result))
(defun created (result) (text-content result))
(defun last-mod (result) (text-content result))

(defun organizer (result)
  (destructuring-bind (group name params value) result
    (declare (ignore group params))
    (make-cal-address-node name value)))

(defun priority (result) (text-content result))
(defun seq (result) (text-content result))
(defun status (result) (text-content result))
(defun transp (result) (text-content result))
(defun recurid (result) (text-content result))

(def-generic-property uid "uid" nil "text")

;; 3.6.1 Event Component
(defparameter *vevent-content-dispatch*
  (let ((hash (make-hash-table :test 'equal)))
    (map nil (lambda (x)
               (setf (gethash (symbol-name x) hash) (symbol-function x)))
         '(dtstamp dtstart uid
           class created description 
           geo 
           last-mod location organizer
           priority seq
           #+nil status-event
           transp
           #+nil url
           recurid 

           #+nil rrule

           dtend 
           #+nil duration
           attach
           attendee
           categories))
    hash))

(defun handle-vevent-content-line (result)
  (destructuring-bind (group name params value) result
       (declare (ignore group params value))
    (let ((fn (gethash (string-upcase name) *vevent-content-dispatch*)))
      (when fn (funcall fn result)))))

(defun vevent? ()
  (named-seq?
   "BEGIN" ":" "VEVENT" #\Return #\Newline
   (<- content (many? (content-line?)))
   "END" ":" "VEVENT" #\Return #\Newline
   (stp:append-child
    (stp:make-element "vevent" *ical-namespace*)
    (let ((vevent
            (reduce (lambda (element x)
                      (let ((x (handle-vevent-content-line x)))
                        (if (and x (not (consp x)))
                            (stp:append-child element x)
                            element)))
                    content
                    :initial-value
                    (stp:make-element "properties" *ical-namespace*))))
      ;; FIXME! We should create DTSTAMP and UID if they don't exist here!
      vevent))))

(defun vtodo? ()
  (named-seq?
   "BEGIN" ":" "VTODO" #\Return #\Newline
   (<- content (many? (content-line?)))
   "END" ":" "VTODO" #\Return #\Newline
   (reduce (lambda (element x)
             (let ((x (handle-content-line x)))
               (if (and x (not (consp x)))
                   (stp:append-child element x)
                   element)))
           content
           :initial-value (stp:make-element "vtodo" *ical-namespace*))))

(defun vjournal? ()
  (named-seq?
   "BEGIN" ":" "VJOURNAL" #\Return #\Newline
   (<- content (many? (content-line?)))
   "END" ":" "VJOURNAL" #\Return #\Newline
   (reduce (lambda (element x)
             (let ((x (handle-content-line x)))
               (if (and x (not (consp x)))
                   (stp:append-child element x)
                   element)))
           content
           :initial-value (stp:make-element "vjournal" *ical-namespace*))))

(defun vfreebusy? ()
  (named-seq?
   "BEGIN" ":" "VFREEBUSY" #\Return #\Newline
   (<- content (many? (content-line?)))
   "END" ":" "VFREEBUSY" #\Return #\Newline
   (reduce (lambda (element x)
             (let ((x (handle-content-line x)))
               (if (and x (not (consp x)))
                   (stp:append-child element x)
                   element)))
           content
           :initial-value (stp:make-element "vfreebusy" *ical-namespace*))))

(defun vtimezone? ()
  (named-seq?
   "BEGIN" ":" "VTIMEZONE" #\Return #\Newline
   (<- content (many? (content-line?)))
   "END" ":" "VTIMEZONE" #\Return #\Newline
   (reduce (lambda (element x)
             (let ((x (handle-content-line x)))
               (if (and x (not (consp x)))
                   (stp:append-child element x)
                   element)))
           content
           :initial-value (stp:make-element "vtimezone" *ical-namespace*))))

(defun component? ()
  (choices (vevent?)
           (vtodo?) 
           (vjournal?)
           (vfreebusy?)
           (vtimezone?)))

(defun calscale (result) (text-content result))
(defun method (result) (text-content result))

(defun calprop? ()
  (content-line?))

(defparameter *content-dispatch*
  (let ((hash (make-hash-table :test 'equal)))
    (map nil (lambda (x)
               (setf (gethash (symbol-name x) hash) (symbol-function x)))
         '(dtstamp dtstart dtend attendee class created description last-mod
           location organizer priority seq status transp recurid uid))
    hash))

(defun handle-content-line (result)
  (destructuring-bind (group name params value) result
       (declare (ignore group params value))
    (let ((fn (gethash (string-upcase name) *content-dispatch*)))
      (when fn (funcall fn result)))))

(defun vcalendar? ()
  (named-seq?
   "BEGIN" ":" "VCALENDAR" #\Return #\Newline
   (<- calprops (many? (calprop?)))
   (<- content (many1? (component?)))
   "END" ":" "VCALENDAR" #\Return #\Newline
   
   (stp:append-child
    (stp:append-child
     (stp:make-element "vcalendar" *ical-namespace*)
     ;; FIXME! We're not really parsing the properties, just cheating here!
     (stp:append-child
      (stp:append-child
       (stp:make-element "properties" *ical-namespace*)
       (stp:append-child
        (stp:make-element "version" *ical-namespace*)
        (make-text-nodes "text" "2.0")))
      (stp:append-child
       (stp:make-element "prodid" *ical-namespace*)
       (make-text-nodes "text" "bar"))))
    (reduce (lambda (element x)
              (if (and x (not (consp x)))
                  (stp:append-child element x)
                  element))
            content
            :initial-value
            (stp:make-element "components" *ical-namespace*)))))

(defun icalendar? ()
  (many1? (named-seq?
           (<- vcal (vcalendar?))
           (many? (crlf?))
           vcal)))

(defun parse-icalendar (str)
  (let ((*default-namespace* *ical-namespace*))
    (stp:make-document
     (reduce #'stp:append-child
             (parse-string* (icalendar?) str)
             :initial-value (stp:make-element "icalendar" *ical-namespace*)))))
