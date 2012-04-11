
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

(defmacro def-generic-parameter (function-name param-name
                                 &key (node-type "text")
                                      allowed-values)
  `(defun ,function-name (params)
     (when-let (param-values
                (mapcan #'second (keep ,param-name params :test #'string-equal :key #'car)))
       (reduce (lambda (parent param-value)
                 (when ,allowed-values
                   (unless (member param-value ,allowed-values :test #'string=)
                     (warn "Unknown encoding ~A parameter: ~A" ,param-name param-value)))
                 (stp:append-child parent (make-text-node ,node-type param-value)))
               param-values
               :initial-value (stp:make-element ,param-name *ical-namespace*)))))


;; 3.2.1 Alternate Text Representation
(def-generic-parameter altrepparam "altrep" :node-type "uri")

;; 3.2.2 Common Name
(def-generic-parameter cnparam "cn" :node-type "text")

;; 3.2.3 Calendar User Type
(def-generic-parameter cutypeparam "cutype"
  :node-type "text"
  :allowed-values '("INDIVIDUAL" 
                    "GROUP" 
                    "RESOURCE"
                    "ROOM"
                    "UNKNOWN"))

;; 3.2.4 Delegators
(def-generic-parameter delfromparam "delegated-from" :node-type "cal-address")

;; 3.2.5 Delegatees
(def-generic-parameter deltoparam "delegated-to" :node-type "cal-address")

;; 3.2.6 
(def-generic-parameter dirparam "dir" :node-type "uri")

;; 3.2.7 Inline Encoding
(def-generic-parameter encodingparam "encoding"
  :node-type "text"
  :allowed-values '("8BIT" "BASE64"))

;; 3.2.8 Format Type
(def-generic-parameter fmttypeparam "fmttype" :node-type "text")

;; 3.2.9 Free/Busy Time Type
(def-generic-parameter fbtypeparam "fbtype"
  :node-type "text"
  :allowed-values '("FREE" 
                    "BUSY" 
                    "BUSY-UNAVAILABLE" 
                    "BUSY-TENTATIVE"))
 
;; 3.2.10 Language
(def-generic-parameter languageparam "language" :node-type "text")

;; 3.2.11 Group or List Membership
(def-generic-parameter memberparam "member" :node-type "cal-address")

;; 3.2.12 FIXME TODO Participation Status

;; 3.2.13 Recurrence Identifier Range
(def-generic-parameter rangeparam "range"
  :node-type "text"
  :allowed-values '("THISANDFUTURE"))

;; 3.2.14 Alarm Trigger Relationship
(def-generic-parameter trigrelparam "related"
  :node-type "text"
  :allowed-values '("START" "END"))

;; 3.2.15 Relationship Type
(def-generic-parameter reltypeparam "reltype"
  :node-type "text"
  :allowed-values '("PARENT"
                    "CHILD"
                    "SIBLING"))

;; 3.2.16 Participation Role
(def-generic-parameter roleparam "role"
  :node-type "text"
  :allowed-values '("CHAIR"
                    "REQ-PARTICIPANT"
                    "OPT-PARTICIPANT"
                    "NON-PARTICIPANT"))
;; 3.2.17 RSVP Expectation
(def-generic-parameter rsvpparam "rsvp" :node-type "boolean")

;; 3.2.18 Sent By
(def-generic-parameter sentbyparam "sent-by" :node-type "cal-address")

;; 3.2.19 Time Zone Identifier
(def-generic-parameter tzidparam "tzid" :node-type "text")


;; 3.6 Calendar Components

;; FIXME these next two should get moved down to the properties section!
(defun calscale (result) (text-content result))
(defun method (result) (text-content result))

;; FIXME flesh out calprop support
(defun calprop? ()
  (content-line?))

(defun component? ()
  (choices (vevent?)
           (vtodo?) 
           (vjournal?)
           (vfreebusy?)
           (vtimezone?)))

;; 3.6.1 Event Component
(defparameter *vevent-content-dispatch*
  (let ((hash (make-hash-table :test 'equal)))
    (map nil (lambda (x)
               (setf (gethash (car x) hash) (cdr x)))
         '((dtstamp . property-dtstamp)
           (dtstart . property-dtstart)
           (uid . property-uid)

           (class .  property-class)
           (created . property-created)
           (description . property-description) 
           (geo . property-geo) 
           (last-mod . property-last-mod)
           (location . property-location)
           (organizer . property-organizer)
           (priority . property-priority)
           (seq . property-seq)
           #+nil status-event
           (summary . property-summary)
           (transp . property-transp)
           (url . property-url)
           #+il (recurid . recurid) 
           #+nil rrule
           (dtend . property-dtend) 
           #+nil duration
           (attach . property-attach)
           (attendee . property-attendee)
           (categories . property-categories)
           (comment . property-comment)
           (contact . property-contact)
           #+nil exdate
           #+nil rstatus
           (related . property-related)
           (resources . property-resources])
           #+nil rdate))
    hash))

(defun handle-vevent-content-line (result)
  (destructuring-bind (group name params value) result
       (declare (ignore group params value))
    (let ((fn (gethash (intern (string-upcase name) :soiree-icalendar)
                       *vevent-content-dispatch*)))
      (when fn
        (funcall fn result)))))

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


;;; Properties

;; Property utility functions

(defun add-params (param-list params)
  (remove nil (mapcar (lambda (x) (funcall x params)) param-list) :test 'eq))

(defun extract-parameters (params functions)
  (let ((param-element (stp:make-element "parameters" *ical-namespace*)))
    (let ((param-children (add-params functions params)))
      (reduce #'stp:append-child param-children :initial-value param-element))))

;; FIXME!!! Check :allowed-values
(defmacro def-generic-property (property-name element-name
                                parameter-functions value-node-type
                                &key multiple-values allowed-values)
  (declare (ignore allowed-values))
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
(defun property-attach (result)
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
(def-generic-property property-categories "categories"
  '(languageparam) "text"
  :multiple-values t)

;; 3.8.1.3 Classification
(def-generic-property property-class "class" nil "text")

;; 3.8.1.4 Comment
(def-generic-property property-comment "comment"
  '(altrepparam languageparam) "text")

;; 3.8.1.5 Description
(def-generic-property property-description "description"
  '(altrepparam languageparam) "text")

;; 3.8.1.6 Geographic Position
(defun property-geo (result)
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
(def-generic-property property-location "location"
  '(altrepparam languageparam) "text")

;; 3.8.1.8 Percent Complete
(def-generic-property property-percent "percent-complete" nil "integer")

;; 3.8.1.9 Priority
(def-generic-property property-priority "priority" nil "integer")

;; 3.8.1.10 Resources
(def-generic-property property-resources "resources"
  '(altrepparam languageparam) "text")

;; 3.8.1.11 Status
(def-generic-property property-status-event "status"
  nil "text" :allowed-values '("TENTATIVE"
                               "CONFIRMED"
                               "CANCELLED"))

(def-generic-property property-status-todo "status"
  nil "text" :allowed-values '("NEEDS-ACTION"
                               "COMPLETED"
                               "IN-PROCESS"
                               "CANCELLED"))

(def-generic-property property-status-jour "status"
  nil "text" :allowed-values '("DRAFT"
                               "FINAL" 
                               "CANCELLED"))

;; 3.8.1.12 Summary
(def-generic-property property-summary "summary"
  '(altrepparam languageparam) "text")

;; 3.8.2 Date and Time Component Properties

;;; Support Functions
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


;; 3.8.2.1 Completed
(defun property-completed (result) (date-time-node result))

;; 3.8.2.2 Date/Time End
(defun property-dtend (result) (date-time-or-date-node result))

;; 3.8.2.3 Ddate/Time Due
(defun property-due (result) (date-time-or-date-node result))

;; 3.8.2.4 Date/Time Start
(defun property-dtstart (result) (date-time-or-date-node result))

;; 3.8.2.5 Duration FIXME!!!
#+nil (defun property-duration (result) (date-time-or-date-node result))

;; 3.8.2.6 Free/Busy Time FIXME!!!

;; 3.8.2.7 Time Transparency
(def-generic-property property-transp "transp" nil "text"
  :allowed-values '("OPAQUE"
                    "TRANSPARENT"))

;; 3.8.3 Time Zone Component Properties

;; 3.8.3.1 Time Zone Identifier
(def-generic-property property-tzid "tzid" nil "text")

;; 3.8.3.2 Time Zone Name
(def-generic-property property-tzname "tzname" '(language-param) "text")

;; 3.8.3.3 Time Zone Offset From
(def-generic-property property-tzoffsetfrom "tzoffsetfrom" nil "utc-offset")

;; 3.8.3.4 Time Zone Offset To
(def-generic-property property-tzoffsetto "tzoffsetto" nil "utc-offset")

;; 3.8.3.5 Time Zone URL
(def-generic-property property-tzurl "tzurl" nil "text")

;; 3.8.4 Relationship Component Properties

;; 3.8.4.1 Attendee
(def-generic-property property-attendee "attendee"
  '(cutypeparam
    memberparam
    roleparam
    ;; FIXME!!!
    #+nil partstatparam
    rsvpparam
    deltoparam
    delfromparam
    sentbyparam
    cnparam
    dirparam
    languageparam)
  "cal-address")

;; 3.8.4.2 Contact
(def-generic-property property-contact "contact"
  '(altrepparam
    languageparam)
  "cal-address")

;; 3.8.4.3 Organzizer
(def-generic-property property-organizer "organizer"
  '(cnparam
    dirparam
    sentbyparam
    languageparam)
  "cal-address")

;; 3.8.4.4 Recurrence ID
;; FIXME!!! This needs help.
#+nil (defun property-recurid (result) (date-time-or-date-node result))

;; 3.8.4.5 Related-To
(def-generic-property property-related "related-to"
  '(reltypeparam)
  "text")

;; 3.8.4.6 Uniform Resource Locator
(def-generic-property property-url "url" nil "uri")

;; 3.8.4.7 Unique Identifier
(def-generic-property property-uid "uid" nil "text")

;; 3.8.5 Recurrence Component Properties

;; 3.8.5.1 Exception Date/Times TBD
;; 3.8.5.2 Recurrence Date/Times TBD
;; 3.8.5.3 Recurrence Rule TBD

;; 3.8.6 Alarm Component Properties

;; 3.8.6.1 Action
(def-generic-property property-action "action" nil "text"
  :allowed-values '("AUDIO"
                    "DISPLAY"
                    "EMAIL"))
;; 3.8.6.2 Repeat Count
(def-generic-property property-repeat "repeat" nil "integer")

;; 3.8.6.3 Trigger TBD


;; 3.8.7 Change Management Component Properties

;; 3.8.7.1 Date/Time Created
(defun property-created (result) (date-time-node result))

;; 3.8.7.2 Date/Time Stamp
(defun property-dtstamp (result) (date-time-node result))

;; 3.8.7.3 Last Modified
(defun property-last-mod (result) (date-time-node result))

;; 3.8.7.4 Sequence Number
(def-generic-property property-seq "sequence" nil "integer")


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

(defun organizer (result)
  (destructuring-bind (group name params value) result
    (declare (ignore group params))
    (make-cal-address-node name value)))

(defun seq (result) (text-content result))

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

(defparameter *content-dispatch*
  (let ((hash (make-hash-table :test 'equal)))
    (map nil (lambda (x)
               (setf (gethash (symbol-name x) hash) x))
         '(property-dtstamp property-dtstart property-dtend
           property-attendee property-class property-created
           property-description property-last-mod
           property-location property-organizer property-priority
           property-seq property-transp #+nil recurid property-uid))
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
