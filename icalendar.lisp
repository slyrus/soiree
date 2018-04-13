
(cl:defpackage :soiree-icalendar
  (:use :common-lisp :parser-combinators :soiree :soiree-parse)
  (:shadow #:standard #:member #:method #:make-method #:class #:sequence #:make-sequence))

(cl:in-package :soiree-icalendar)

(defvar *ical-namespace* "urn:ietf:params:xml:ns:icalendar-2.0")

(defvar *ical-rng-pathname*
  (merge-pathnames #p"icalendar-2.0.rnc" soiree-config:*base-directory*))

(defvar *ical-rng-schema* (cxml-rng:parse-compact *ical-rng-pathname*))



;;
;; icalendar classes

;; icalendar-document
(defclass icalendar-document (stp:document) ())

(defun make-icalendar-document (icalendar)
  (change-class (stp:make-document icalendar) 'icalendar-document))

(defmethod icalendar ((icalendar-document icalendar-document ))
  (stp:nth-child 0 icalendar-document)

  #+nil
  (stp:find-child "icalendar" icalendar-document :key #'stp:local-name :test #'equal))

(defmacro defelement (elt-name)
  `(progn
    (defclass ,elt-name (stp:element) ())

    (defun ,(intern (string-upcase (concatenate 'string "make-" (symbol-name elt-name))) *package*) ()
      (change-class
       ;; FIXME! Ideally we wouldn't need the coerce here, but we
       ;; can't just pass a simple-base-string (which is apparently
       ;; what we get back from symbol-name) to stp:make-element as it
       ;; wants a proper vector of characters, not base-chars.
       (stp:make-element ,(coerce (string-downcase (symbol-name elt-name)) 'runes:rod) *ical-namespace*)
       ',elt-name))))

;; icalendar element
(defelement icalendar)

(defmethod vcalendar ((icalendar icalendar))
  (stp:find-child "vcalendar" icalendar))

(defmethod vcalendar+ ((icalendar icalendar))
  (stp:list-children icalendar))

;; vcalendar element
(defelement vcalendar)

(defmethod properties ((vcalendar vcalendar))
  (stp:find-child "properties" vcalendar :key #'stp:local-name :test #'equal))

(defmethod components ((vcalendar vcalendar))
  (stp:find-child "components" vcalendar :key #'stp:local-name :test #'equal))

(defmethod vtimezone+ ((components components))
  (stp:filter-children (lambda (x) (equal x "vtimezone")) components :key #'stp:local-name))

(defmethod properties ((vtimezone vtimezone))
  (stp:find-child "properties" vtimezone :key #'stp:local-name :test #'equal))

(defmethod tzid ((properties properties))
  (stp:find-child "tzid" properties :key #'stp:local-name :test #'equal))

(defmethod text ((tzid tzid))
  (stp:find-child "text" tzid :key #'stp:local-name :test #'equal))

;; properties element
(defelement properties)

(defmethod version ((properties properties))
  (stp:find-child "version" properties :key #'stp:local-name :test #'equal))

(defmethod prodid ((properties properties))
  (stp:find-child "prodid" properties :key #'stp:local-name :test #'equal))

(defmethod clascale ((properties properties))
  (stp:find-child "clascale" properties :key #'stp:local-name :test #'equal))

;; components element
(defelement components)

;; version element
(defelement version)

;; components element
(defelement prodid)

;; components element
(defelement calscale)




;; Value type utility functions
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

(defun utc-offset? ()
  (hook? (lambda (x)
           (destructuring-bind (sign hour-digits minute-digits second-digits) x
             (let ((hour (digits-to-number hour-digits))
                   (minute (+ (* 10 (first minute-digits)) (second minute-digits)))
                   (second (when second-digits
                             (+ (* 10 (first second-digits)) (second second-digits)))))
               (list (if (eq sign #\-)
                         (- hour)
                         hour)
                     minute second))))
         (seq-list? (opt? (choice1 #\+ #\-))
                    (times? (hook? #'digit-char-p (digit?)) 2)
                    (times? (hook? #'digit-char-p (digit?)) 2)
                    (opt? (times? (hook? #'digit-char-p (digit?)) 2)))))

;;; Parameters

(defmacro def-generic-parameter (function-name param-name
                                 &key (node-type "text")
                                      allowed-values)
  `(let ((constructor (defelement ,(intern (string-upcase param-name)))))
    (defun ,function-name (params)
      (when-let (param-values
                 (mapcan #'second (keep ,param-name params :test #'string-equal :key #'car)))
        (reduce (lambda (parent param-value)
                  (when ,allowed-values
                    (unless (cl:member param-value ,allowed-values :test #'string=)
                      (warn "Unknown encoding ~A parameter: ~A" ,param-name param-value)))
                  (stp:append-child parent (make-text-node ,node-type param-value)))
                param-values
                :initial-value (funcall constructor))))))


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

;; 3.2.12 Participation Status
(def-generic-parameter partstatparam "partstat"
  :node-type "text"
  :allowed-values '("NEEDS-ACTION"
                    "ACCEPTED"
                    "DECLINED"
                    "TENTATIVE"
                    "DELEGATED"
                    "COMPLETED"
                    "IN-PROCESS"))

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

;; 3.3 Property Value Datatypes

;; 3.3.4 DATE
(defun date? ()
  (hook? (lambda (x)
           (destructuring-bind (year-digits month-digits day-digits) x
             (let ((year (digits-to-number year-digits))
                   (month (+ (* 10 (first month-digits)) (second month-digits)))
                   (day (+ (* 10 (first day-digits)) (second day-digits))))
               (list year month day))))
         (seq-list? (times? (hook? #'digit-char-p (digit?)) 4)
                    (times? (hook? #'digit-char-p (digit?)) 2)
                    (times? (hook? #'digit-char-p (digit?)) 2))))

;; 3.3.5 DATE-TIME
(defun time? ()
  (hook? (lambda (x)
           (destructuring-bind (hour-digits minute-digits second-digits time-utc) x
             (let ((hour (digits-to-number hour-digits))
                   (minute (+ (* 10 (first minute-digits)) (second minute-digits)))
                   (second (+ (* 10 (first second-digits)) (second second-digits))))
               (list hour minute second time-utc))))
         (seq-list? (times? (hook? #'digit-char-p (digit?)) 2)
                    (times? (hook? #'digit-char-p (digit?)) 2)
                    (times? (hook? #'digit-char-p (digit?)) 2)
                    (opt? #\Z))))

(defun date-time? ()
  (named-seq?
   (<- date (date?))
   #\T
   (<- time (time?))
   (list date time)))

;; 3.3.6 DURATION
(defun duration-second? ()
  (named-seq?
   (<- seconds (nat?))
   #\S
   (list :seconds seconds)))

(defun duration-minute? ()
  (hook? (lambda (x)
           (destructuring-bind (minutes dur-second) x
             (append (list :minutes minutes)
                     dur-second)))
         (named-seq?
          (<- minutes (nat?))
          #\M
          (<- seconds (opt? (duration-second?)))
          (list minutes seconds))))

(defun duration-hour? ()
  (hook? (lambda (x)
           (destructuring-bind (hours dur-minute) x
             (append (list :hours hours) dur-minute)))
         (named-seq?
          (<- hours (nat?))
          #\H
          (<- minutes (opt? (duration-minute?)))
          (list hours minutes))))

(defun duration-time? ()
  (named-seq?
   #\T
   (<- dur-time (choices (duration-hour?)
                         (duration-minute?)
                         (duration-second?)))
   dur-time))

(defun duration? ()
  (hook? (lambda (x)
           (destructuring-bind (negative dur-weeks-days-or-time)
               x
             (list negative dur-weeks-days-or-time)))
         (named-seq?
          (<- negative (hook? (lambda (x) (when (eq x #\-) t))
                              (opt? (choice #\+ #\-))))
          #\P
          (<- dur-weeks-days-or-time
              (choices
               (hook? (lambda (dur-weeks)
                        (list :weeks dur-weeks))
                      (named-seq?
                       (<- dur-weeks (nat?))
                       #\W
                       dur-weeks))
               (hook? (lambda (x)
                        (destructuring-bind (dur-days dur-time)
                            x
                          (list :days dur-days :time dur-time)))
                      (named-seq?
                       (<- dur-days (nat?))
                       #\D
                       (<- dur-time (opt? (duration-time?)))
                       (list dur-days dur-time)))
               (hook? (lambda (dur-time)
                        (list :time dur-time))
                      (duration-time?))))
          (list negative dur-weeks-days-or-time))))

(defun parse-duration (string)
  (parse-string* (duration?) string))

;; 3.3.9 PERIOD

(defun date-time-or-date? ()
  (choice1 (hook? (lambda (date-time)
                    (list :date-time date-time))
                  (date-time?))
           (hook? (lambda (duration)
                    (list :duration duration))
                  (duration?))))

(defun period? ()
  (named-seq?
   (<- start (date-time?))
   #\/
   (<- end-or-duration (date-time-or-date?))
   (list start end-or-duration)))

(defun parse-period (string)
  (parse-string* (period?) string))

(defelement period)

(defun make-period-node (string)
  (let ((node (make-period)))
    (let ((parsed (parse-period string)))
      (destructuring-bind (start end) parsed
        (destructuring-bind ((year month day)
                             (hours minute second utc))
            start
          ;; FIXME
          (declare (ignore utc))
          (stp:append-child
           node
           (make-text-node "start"
                           (format nil "~2,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D"
                                   year month day hours minute second))))
        (destructuring-bind (&key date-time duration) end
          (cond
            (date-time
             (destructuring-bind ((year month day)
                                  (hours minute second utc))
                 date-time
          ;; FIXME
          (declare (ignore utc))
               (stp:append-child
                node
                (make-text-node "end"
                                (format nil "~2,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D"
                                        year month day hours minute second)))))
            (duration
             (stp:append-child
              node
              (make-text-node
               "duration"
               (destructuring-bind (negative (&key weeks days time))
                   duration
                 (cond
                   (weeks (format nil "~:[~;-~]P~D" negative weeks))
                   (t
                    (destructuring-bind (&key hours minutes seconds) time
                      (format nil "~:[~;-~]P~@[~DD~]~:[~;T~]~@[~DH~]~@[~DM~]~@[~DS~]"
                              negative days
                              (or hours minutes seconds)
                              hours minutes seconds))))))))))))
    node))


;; 3.6 Calendar Components

(defun component? ()
  (choices (vevent?)
           (vtodo?) 
           (vjournal?)
           (vfreebusy?)
           (vtimezone?)))

;; 3.6.1 Event Component
(defparameter *vevent-property-dispatch*
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
           (status . property-status-event)
           (summary . property-summary)
           (transp . property-transp)
           (url . property-url)
           (recurid . recurid) 
           (rrule . property-rrule)
           (dtend . property-dtend) 
           (duration property-duration)
           (attach . property-attach)
           (attendee . property-attendee)
           (categories . property-categories)
           (comment . property-comment)
           (contact . property-contact)
           (exdate . property-exdate)
           (request-status . property-rstatus)
           (related . property-related)
           (resources . property-resources)
           (rdate . property-rdate)))
    hash))

(defun handle-vevent-property-line (result)
  (destructuring-bind (group name params value) result
       (declare (ignore group params value))
    (let ((fn (gethash (intern (string-upcase name) :soiree-icalendar)
                       *vevent-property-dispatch*)))
      (when fn
        (funcall fn result)))))

(defelement vevent)

(defun vevent? ()
  (named-seq?
   "BEGIN" ":" "VEVENT" #\Return #\Newline
   (<- properties (many? (property-line?)))
   (<- components (many? (alarmc?)))
   "END" ":" "VEVENT" #\Return #\Newline
   ;; FIXME! We should create DTSTAMP and UID if they don't exist here!
   (let ((vevent-node (make-vevent)))
     (stp:append-child
      vevent-node
      (reduce (lambda (element x)
                (let ((x (handle-vevent-property-line x)))
                  (if (and x (not (consp x)))
                      (stp:append-child element x)
                      element)))
              properties
              :initial-value (make-properties)))
     (when components
       (stp:append-child
        vevent-node
        (reduce #'stp:append-child
                components
                :initial-value (make-components))))
     vevent-node)))

;; 3.6.2 To-do Component
(defparameter *vtodo-property-dispatch*
  (let ((hash (make-hash-table :test 'equal)))
    (map nil (lambda (x)
               (setf (gethash (car x) hash) (cdr x)))
         '((dtstamp . property-dtstamp)
           (uid . property-uid)

           (class .  property-class)
           (completed .  property-completed)
           (created . property-created)
           (description . property-description) 
           (geo . property-geo) 
           (last-mod . property-last-mod)
           (location . property-location)
           (organizer . property-organizer)
           (percent . property-percent)
           (priority . property-priority)
           (recurid . property-recurid) 
           (seq . property-seq)
           (status . property-status-todo)
           (summary . property-summary)
           (url . property-url)
           (rrule . property-rrule)
           (attach . property-attach)
           (attendee . property-attendee)
           (categories . property-categories)
           (comment . property-comment)
           (contact . property-contact)
           (exdate . property-exdate)
           (request-status . property-rstatus)
           (related . property-related)
           (resources . property-resources)
           (rdate . property-rdate)))
    hash))

(defun handle-vtodo-property-line (result)
  (destructuring-bind (group name params value) result
       (declare (ignore group params value))
    (let ((fn (gethash (intern (string-upcase name) :soiree-icalendar)
                       *vtodo-property-dispatch*)))
      (when fn
        (funcall fn result)))))

(defelement vtodo)

(defun vtodo? ()
  (named-seq?
   "BEGIN" ":" "VTODO" #\Return #\Newline
   (<- properties (many? (property-line?)))
   (<- components (many? (alarmc?)))
   "END" ":" "VTODO" #\Return #\Newline
   (let ((vtodo-node (make-vtodo)))
     (stp:append-child
      vtodo-node
      (reduce (lambda (element x)
                (let ((x (handle-vtodo-property-line x)))
                  (if (and x (not (consp x)))
                      (stp:append-child element x)
                      element)))
              properties
              :initial-value (make-properties)))
     (when components
       (stp:append-child
        vtodo-node
        (reduce #'stp:append-child
                components
                :initial-value (make-components))))
     vtodo-node)))

;; 3.6.3 Journal Component

(defparameter *vjournal-property-dispatch*
  (let ((hash (make-hash-table :test 'equal)))
    (map nil (lambda (x)
               (setf (gethash (car x) hash) (cdr x)))
         '((dtstamp . property-dtstamp)
           (uid . property-uid)
           (class .  property-class)
           (created . property-created)
           (dtstart . property-dtstart)
           (last-mod . property-last-mod)
           (organizer . property-organizer)
           (recurid . property-recurid) 
           (seq . property-seq)
           (status . property-status-journal)
           (summary . property-summary)
           (url . property-url)
           (rrule . property-rrule)
           (attach . property-attach)
           (attendee . property-attendee)
           (categories . property-categories)
           (comment . property-comment)
           (contact . property-contact)
           (description . property-description) 
           (exdate . property-exdate)
           (related . property-related)
           (rdate . property-rdate)
           (request-status . property-rstatus)))
    hash))

(defun handle-vjournal-property-line (result)
  (destructuring-bind (group name params value) result
       (declare (ignore group params value))
    (let ((fn (gethash (intern (string-upcase name) :soiree-icalendar)
                       *vjournal-property-dispatch*)))
      (when fn
        (funcall fn result)))))

(defelement vjournal)

(defun vjournal? ()
  (named-seq?
   "BEGIN" ":" "VJOURNAL" #\Return #\Newline
   (<- properties (many? (property-line?)))
   "END" ":" "VJOURNAL" #\Return #\Newline
   (stp:append-child
    (make-vjournal)
    (reduce (lambda (element x)
              (let ((x (handle-vjournal-property-line x)))
                (if (and x (not (consp x)))
                    (stp:append-child element x)
                    element)))
            properties
            :initial-value (make-properties)))))

;; 3.6.4 Free/Busy Component

(defparameter *vfreebusy-property-dispatch*
  (let ((hash (make-hash-table :test 'equal)))
    (map nil (lambda (x)
               (setf (gethash (car x) hash) (cdr x)))
         '((dtstamp . property-dtstamp)
           (uid . property-uid)
           (contact . property-contact)
           (dtstart . property-dtstart)
           (dtend . property-dtstart)
           (duration property-duration)
           (organizer . property-organizer)
           (url . property-url)
           (attendee . property-attendee)
           (comment . property-comment)
           (request-status . property-rstatus)
           (freebusy . property-freebusy)))
    hash))

(defun handle-vfreebusy-property-line (result)
  (destructuring-bind (group name params value) result
       (declare (ignore group params value))
    (let ((fn (gethash (intern (string-upcase name) :soiree-icalendar)
                       *vfreebusy-property-dispatch*)))
      (when fn
        (funcall fn result)))))

(defelement vfreebusy)

(defun vfreebusy? ()
  (named-seq?
   "BEGIN" ":" "VFREEBUSY" #\Return #\Newline
   (<- properties (many? (property-line?)))
   "END" ":" "VFREEBUSY" #\Return #\Newline
   (stp:append-child
    (make-vfreebusy)
    (reduce (lambda (element x)
              (let ((x (handle-vfreebusy-property-line x)))
                (if (and x (not (consp x)))
                    (stp:append-child element x)
                    element)))
            properties
            :initial-value (make-properties)))))

;; 3.6.5 Time Zone Component

(defparameter *vtimezone-property-dispatch*
  (let ((hash (make-hash-table :test 'equal)))
    (map nil (lambda (x)
               (setf (gethash (car x) hash) (cdr x)))
         '((tzid . property-tzid)
           (lastmod . property-lastmod)
           (tzurl . property-tzurl)))
    hash))

(defun handle-vtimezone-property-line (result)
  (destructuring-bind (group name params value) result
       (declare (ignore group params value))
    (let ((fn (gethash (intern (string-upcase name) :soiree-icalendar)
                       *vtimezone-property-dispatch*)))
      (when fn
        (funcall fn result)))))

(defparameter *tz-property-dispatch*
  (let ((hash (make-hash-table :test 'equal)))
    (map nil (lambda (x)
               (setf (gethash (car x) hash) (cdr x)))
         '((dtstart . property-dtstart)
           (tzoffsetfrom . property-tzoffsetfrom)
           (tzoffsetto . property-tzoffsetto)
           (rrule . property-rrule)
           (comment . property-comment)
           (rdate . property-rdate)
           (tzname . property-tzname)))
    hash))

(defun handle-tz-property-line (result)
  (destructuring-bind (group name params value) result
       (declare (ignore group params value))
    (let ((fn (gethash (intern (string-upcase name) :soiree-icalendar)
                       *tz-property-dispatch*)))
      (when fn
        (funcall fn result)))))

(defelement standard)

(defun standardc? ()
  (named-seq?
   "BEGIN" ":" "STANDARD" #\Return #\Newline
   (<- properties (many? (property-line?)))
   "END" ":" "STANDARD" #\Return #\Newline
   (stp:append-child
    (make-standard)
    (reduce (lambda (element x)
              (let ((x (handle-tz-property-line x)))
                (if (and x (not (consp x)))
                    (stp:append-child element x)
                    element)))
            properties
            :initial-value (make-properties)))))

(defelement daylight)

(defun daylightc? ()
  (named-seq?
   "BEGIN" ":" "DAYLIGHT" #\Return #\Newline
   (<- properties (many? (property-line?)))
   "END" ":" "DAYLIGHT" #\Return #\Newline
   (stp:append-child
    (make-daylight)
    (reduce (lambda (element x)
              (let ((x (handle-tz-property-line x)))
                (if (and x (not (consp x)))
                    (stp:append-child element x)
                    element)))
            properties
            :initial-value (make-properties)))))

(defelement vtimezone)

(defun vtimezone? ()
  (named-seq?
   "BEGIN" ":" "VTIMEZONE" #\Return #\Newline
   (<- properties (many? (property-line?)))
   (<- components (many? (choice (standardc?)
                                 (daylightc?))))
   "END" ":" "VTIMEZONE" #\Return #\Newline
   (reduce
    #'stp:append-child
    (list (reduce (lambda (element x)
                    (let ((x (handle-vtimezone-property-line x)))
                      (if (and x (not (consp x)))
                          (stp:append-child element x)
                          element)))
                  properties
                  :initial-value (make-properties))
          (reduce #'stp:append-child
                  components
                  :initial-value (make-components)))
    :initial-value (make-vtimezone))))

;; 3.6.6 Alarm Component

(defparameter *valarm-property-dispatch*
  (let ((hash (make-hash-table :test 'equal)))
    (map nil (lambda (x)
               (setf (gethash (car x) hash) (cdr x)))
         '((action . property-action)
           (description . property-description)
           (trigger . property-trigger)
           (summary . property-attendee)
           (attendee . property-attendee)
           (duration . property-duration)
           (repeat . property-repeat)
           (attach . property-attach)))
    hash))

(defun handle-valarm-property-line (result)
  (destructuring-bind (group name params value) result
       (declare (ignore group params value))
    (let ((fn (gethash (intern (string-upcase name) :soiree-icalendar)
                       *valarm-property-dispatch*)))
      (when fn
        (funcall fn result)))))

(defelement valarm)

(defun alarmc? ()
  (named-seq?
   "BEGIN" ":" "VALARM" #\Return #\Newline
   (<- properties (many? (property-line?)))
   "END" ":" "VALARM" #\Return #\Newline
   (let ((valarm-node (make-valarm))
         (property-node
           (reduce
            (lambda (element x)
              (let ((x (handle-valarm-property-line x)))
                (if (and x (not (consp x)))
                    (stp:append-child element x)
                    element)))
            properties
            :initial-value
            (make-properties))))
     (stp:append-child valarm-node property-node)
     ;; We need to fix up valarm-node such that if there is a duration
     ;; node, it appears before the repat node!
     (let ((duration (stp:find-child "duration" property-node
                                     :key #'stp:local-name
                                     :test #'equal))
           (repeat (stp:find-child "repeat" property-node
                                   :key #'stp:local-name)))
       (when (and duration repeat)
          (stp:delete-child repeat property-node)
          (stp:insert-child-after property-node repeat duration)))
     valarm-node)))

;;; Properties

;; Property utility functions

(defun add-params (param-list params)
  (remove nil (mapcar (lambda (x) (funcall x params)) param-list) :test 'eq))

(defelement parameters)

(defun extract-parameters (params functions)
  (let ((param-element (make-parameters)))
    (let ((param-children (add-params functions params)))
      (reduce #'stp:append-child param-children :initial-value param-element))))


(defun make-text-node-list (element-tag &rest strings)
  (mapcar (lambda (x)
            (stp:append-child (stp:make-element element-tag *default-namespace*)
                              (stp:make-text x)))
          strings))

(defelement text)

;; FIXME!!! Check :allowed-values
(defmacro def-generic-property (property-name element-name
                                parameter-functions value-node-type
                                &key multiple-values allowed-values)
  (declare (ignore allowed-values value))
  `(let ((constructor (defelement ,(intern (string-upcase element-name)))))
    (defun ,property-name (result)
      (destructuring-bind (group name params value) result
        (declare (ignore group name))
        (let ((node (funcall constructor))
              (param-element (extract-parameters
                              params
                              ,parameter-functions)))
          (when (plusp (stp:number-of-children param-element))
            (stp:append-child node param-element))
          ,(if multiple-values
               `(reduce #'stp:append-child
                 (mapcar (lambda (x)
                           (stp:append-child (make-text)
                                             (stp:make-text x)))
                  (split-string value))
                 :initial-value node)
               `(stp:append-child
                 node
                 (stp:append-child (make-text) (stp:make-text value)))))))))

;; 3.7 Calendar Properties

;; 3.7.1 Calendar Scale
(def-generic-property property-calscale "calscale" nil "text"
  :allowed-values '("GREGORIAN"))

;; 3.7.2 Method
(def-generic-property property-method "method" nil "text")

;; 3.7.3 Product Identifier
(def-generic-property property-prodid "prodid" nil "text")

;; 3.7.4 Version
(def-generic-property property-version "version" nil "text"
  :allowed-values '("2.0"))


;; 3.8 Component Properties

;; 3.8.1 Descriptive Component Properties

;; 3.8.1.1 Attachment
(defelement attach)

(defun property-attach (result)
  (destructuring-bind
      (group name params value) result
    (declare (ignore group name))
    (let ((node (make-attach))
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
(defelement geo)

(defun property-geo (result)
  (destructuring-bind
      (group name params value)
      result
    (declare (ignore group name params))
    (let ((node (make-geo)))
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
(defun convert-icalendar-date-to-xcal (string)
  (parse-string* (date?) string))

(defun convert-icalendar-date-time-to-xcal (string)
  (parse-string* (date-time?) string))

(defun convert-icalendar-utc-offset-to-xcal (string)
  (destructuring-bind (hour minute second)
      (parse-string* (utc-offset?) string)
    (format nil "~:[~;-~]~2,'0D:~2,'0D~@[:~2,'0D~]"
            (minusp hour) (abs hour) minute second)))

(defelement date)

(defun make-date-node (element-tag string)
  (stp:append-child
   (stp:make-element (string-downcase element-tag) *ical-namespace*)
   (stp:append-child
    (make-date)
    (destructuring-bind (year month day)
        (convert-icalendar-date-to-xcal string)
      (stp:make-text (format nil "~2,'0D-~2,'0D-~2,'0D" year month day))))))

(defelement date-time)

(defun %make-date-time-node (string)
  (stp:append-child
   (make-date-time)
   (destructuring-bind ((year month day)
                        (hours minute second utc))
       (convert-icalendar-date-time-to-xcal string)
     ;; FIXME
     (declare (ignore utc))
     (stp:make-text (format nil "~2,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D"
                            year month day hours minute second)))))

(defelement tzid)

(defun make-date-time-node (element-tag string &key tzid)
  (let ((parent (stp:make-element (string-downcase element-tag) *ical-namespace*)))
    (when tzid
      (let ((param-element (make-parameters)))
        (let ((tzid-element (make-tzid)))
          (stp:append-child tzid-element (stp:make-text tzid))
          (stp:append-child param-element tzid-element))
        (stp:append-child parent param-element)))
    (stp:append-child parent (%make-date-time-node string))
    parent))

(defelement utc-offset)

(defun %make-utc-node (string)
  (stp:append-child
   (make-utc-offset)
   (stp:make-text (convert-icalendar-utc-offset-to-xcal string))))

;; FIXME! Add tz support here!
(defun date-time-node (result)
  (destructuring-bind (group name params value) result
    (declare (ignore group params))
    (make-date-time-node name value)))

;; FIXME! Add tz support here!
(defun date-time-or-date-node (result)
  (destructuring-bind (group name params value) result
    (declare (ignore group))
    (let ((tzid (caadar (keep "tzid" params :test #'string-equal :key #'car))))
      (when tzid
        (print (list 'bogus tzid)))
      (let ((value-type
             (or (caadar (keep "value" params :test #'string-equal :key #'car)))))
        (cond ((string-equal value-type "DATE")
               (make-date-node name value))
              (t
               (apply #'make-date-time-node name value
                      (if tzid `(:tzid ,tzid)))))))))

(defmacro def-date-time-or-date-property (property-name element-name
                                          parameter-functions)
  `(defun ,property-name (result)
     (destructuring-bind (group name params value) result
       (declare (ignore group name))
       (let ((node (stp:make-element ,element-name *ical-namespace*))
             (param-element (extract-parameters
                             params
                             ,parameter-functions)))
         (when (plusp (stp:number-of-children param-element))
           (stp:append-child node param-element))
         (let ((value-type
                 (or (caadar (keep "value" params :test #'string-equal :key #'car)))))
           (cond ((string-equal value-type "DATE")
                  (make-date-node ,element-name value))
                 (t
                   (make-date-time-node ,element-name value))))))))

;; 3.8.2.1 Completed
(defun property-completed (result) (date-time-node result))

;; 3.8.2.2 Date/Time End
(defun property-dtend (result) (date-time-or-date-node result))

;; 3.8.2.3 Ddate/Time Due
(defun property-due (result) (date-time-or-date-node result))

;; 3.8.2.4 Date/Time Start
(defun property-dtstart (result) (date-time-or-date-node result))

;; 3.8.2.5 Duration
(def-generic-property property-duration "duration" nil "duration")

;; 3.8.2.6 Free/Busy Time

(defelement freebusy)

(defun property-freebusy (result)
  (destructuring-bind
      (group name params value)
      result
    (declare (ignore group name))
    (let ((node (make-freebusy))
          (param-element (extract-parameters params '(fbtypeparam))))
      (when (plusp (cxml-stp:number-of-children param-element))
        (cxml-stp:append-child node param-element))
      (reduce #'cxml-stp:append-child
              (mapcar #'make-period-node (split-string value))
              :initial-value node))))

;; 3.8.2.7 Time Transparency
(def-generic-property property-transp "transp" nil "text"
  :allowed-values '("OPAQUE"
                    "TRANSPARENT"))

;; 3.8.3 Time Zone Component Properties

;; 3.8.3.1 Time Zone Identifier
(def-generic-property property-tzid "tzid" nil "text")

;; 3.8.3.2 Time Zone Name
(def-generic-property property-tzname "tzname" '(languageparam) "text")

;; 3.8.3.3 Time Zone Offset From
(defelement tzoffsetfrom)

(defun property-tzoffsetfrom (result)
  (destructuring-bind
      (group name params value)
      result
    (declare (ignore group name))
    (let ((node (make-tzoffsetfrom))
          (param-element (extract-parameters params nil)))
      (when (plusp (cxml-stp:number-of-children param-element))
        (cxml-stp:append-child node param-element))
      (stp:append-child node (%make-utc-node value)))))

;; 3.8.3.4 Time Zone Offset To
(defelement tzoffsetto)

(defun property-tzoffsetto (result)
  (destructuring-bind
      (group name params value)
      result
    (declare (ignore group name))
    (let ((node (make-tzoffsetto))
          (param-element (extract-parameters params nil)))
      (when (plusp (cxml-stp:number-of-children param-element))
        (cxml-stp:append-child node param-element))
      (stp:append-child node (%make-utc-node value)))))

;; 3.8.3.5 Time Zone URL
(def-generic-property property-tzurl "tzurl" nil "uri")

;; 3.8.4 Relationship Component Properties

;; 3.8.4.1 Attendee
(def-generic-property property-attendee "attendee"
  '(cutypeparam
    memberparam
    roleparam
    partstatparam
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
(def-date-time-or-date-property property-recurid "recurid"
  '(tzidparam rangeparam))

;; 3.8.4.5 Related-To
(def-generic-property property-related "related-to"
  '(reltypeparam)
  "text")

;; 3.8.4.6 Uniform Resource Locator
(def-generic-property property-url "url" nil "uri")

;; 3.8.4.7 Unique Identifier
(def-generic-property property-uid "uid" nil "text")

;; 3.8.5 Recurrence Component Properties

;; 3.8.5.1 Exception Date/Times
(def-date-time-or-date-property property-exdate "exdate" '(tzidparam))

;; 3.8.5.2 Recurrence Date/Times
(defelement rdate)

(defun property-rdate (result)
  (destructuring-bind (group name params value) result
    (declare (ignore group name))
    (let ((node (make-rdate))
          (param-element (extract-parameters params '(tzidparam))))
      (when (plusp (cxml-stp:number-of-children param-element))
        (cxml-stp:append-child node param-element))
      (let ((value-type
             (or
              (caadar (keep "value" params :test #'string-equal :key #'car)))))
        (cond ((string-equal value-type "date")
               (make-date-node "rdate" value))
              ((string-equal value-type "period")
               (stp:append-child
                (make-rdate)
                (make-period-node value)))
              (t (make-date-time-node "rdate" value)))))))

;; 3.8.5.3 Recurrence Rule
(defun parse-value-recur (string)
  (let ((rules (split-string string :delimiter #\;)))
    (let ((pairs
            (mapcan (lambda (x)
                      (destructuring-bind (pair-key pair-value)
                          (split-string x :delimiter #\=)
                        (list (intern (string-upcase pair-key) :keyword)
                              pair-value)))
                    rules)))
      (destructuring-bind
          (&key freq until count interval bysecond byminute
                byhour byday bymonthday byyearday byweekno
                bymonth bysetpos wkst)
          pairs
        (append
         (when freq (list (make-text-node "freq" freq)))
         (when until
           (destructuring-bind (&key date-time date)
               (parse-string* (date-time-or-date?) until)
             (cond (date (list (make-date-node "until" until)))
                   (date-time (list (make-date-time-node "until" until))))))
         (when count (list (make-text-node "count" count)))
         (when interval (list (make-text-node "interval" interval)))
         (when bysecond (list (make-text-node "bysecond" bysecond)))
         (when byminute (list (make-text-node "byminute" byminute)))
         (when byhour (list (make-text-node "byhour" byhour)))
         (when byday (list (make-text-node "byday" byday)))
         (when bymonthday (list (make-text-node "bymonthday" bymonthday)))
         (when byyearday (list (make-text-node "byyearday" byyearday)))
         (when byweekno (list (make-text-node "byweekno" byweekno)))
         (when bymonth (list (make-text-node "bymonth" bymonth)))
         (when bysetpos (list (make-text-node "bysetpos" bysetpos)))
         (when wkst (list (make-text-node "wkst" wkst))))))))

(defelement rrule)
(defelement recur)

(defun property-rrule (result)
  (destructuring-bind (group name params value) result
    (declare (ignore group name))
    (let ((node (make-rrule))
          (param-element (extract-parameters params nil)))
      (when (plusp (cxml-stp:number-of-children param-element))
        (cxml-stp:append-child node param-element))
      (when-let (rules (parse-value-recur value))
        (stp:append-child
         node
         (reduce #'stp:append-child
                 (parse-value-recur value)
                 :initial-value (make-recur))))
      node)))

;; 3.8.6 Alarm Component Properties

;; 3.8.6.1 Action
(def-generic-property property-action "action" nil "text"
  :allowed-values '("AUDIO"
                    "DISPLAY"
                    "EMAIL"))
;; 3.8.6.2 Repeat Count
(def-generic-property property-repeat "repeat" nil "integer")

;; 3.8.6.3 Trigger
(defelement trigger)

(defun property-trigger (result)
  (destructuring-bind
      (group name params value)
      result
    (declare (ignore group name))
    (let ((node (make-trigger))
          (param-element (extract-parameters params nil)))
      (when (plusp (cxml-stp:number-of-children param-element))
        (cxml-stp:append-child node param-element))
      (let ((trigger-type
              (caadar
               (keep "value" params :test #'string-equal :key #'car))))
        (if (string-equal trigger-type "date-time")
            (cxml-stp:append-child node (%make-date-time-node value))
            (cxml-stp:append-child node (make-text-node "duration" value)))))))

;; 3.8.7 Change Management Component Properties

;; 3.8.7.1 Date/Time Created
(defun property-created (result) (date-time-node result))

;; 3.8.7.2 Date/Time Stamp
(defun property-dtstamp (result) (date-time-node result))

;; 3.8.7.3 Last Modified
(defun property-last-mod (result) (date-time-node result))

;; 3.8.7.4 Sequence Number
(def-generic-property property-seq "sequence" nil "integer")

;; 3.8.8.3 Request Status
(defun parse-request-status (value)
  "Returns a list of three nodes: code, description and data."
  (destructuring-bind (code desc &optional data)
      (split-string value :delimiter #\;)
    (list (make-text-node "code" code)
          (make-text-node "description" desc)
          (make-text-node "data" data))))

(defelement request-status)

(defun property-rstatus (result)
  (destructuring-bind (group name params value) result
    (declare (ignore group name))
    (let ((node (make-request-status))
          (param-element (extract-parameters params '(languageparam))))
      (when (plusp (cxml-stp:number-of-children param-element))
        (cxml-stp:append-child node param-element))
      (reduce #'stp:append-child
              (parse-request-status value)
              :initial-value node))))


(defparameter *vcalendar-property-dispatch*
  (let ((hash (make-hash-table :test 'equal)))
    (map nil (lambda (x)
               (setf (gethash (car x) hash) (cdr x)))
         '((prodid . property-prodid)
           (version . property-version)
           (calscale . property-calscale)
           (method . property-method)
           ))
    hash))

(defun handle-vcalendar-property-line (result)
  (destructuring-bind (group name params value) result
       (declare (ignore group params value))
    (let ((fn (gethash (intern (string-upcase name) :soiree-icalendar)
                       *vcalendar-property-dispatch*)))
      (when fn
        (funcall fn result)))))


;;; Main Calendar Parsing Entry
(defun vcalendar? ()
  (named-seq?
   "BEGIN" ":" "VCALENDAR" #\Return #\Newline
   (<- properties (many? (property-line?)))
   (<- components (many1? (component?)))
   "END" ":" "VCALENDAR" #\Return #\Newline
   (let ((vcalendar-node (make-vcalendar)))
     (stp:append-child
      vcalendar-node
      (reduce (lambda (element x)
                (let ((x (handle-vcalendar-property-line x)))
                  (if (and x (not (consp x)))
                      (stp:append-child element x)
                      element)))
              properties
              :initial-value (make-properties)))
     (stp:append-child
      vcalendar-node
      (reduce (lambda (element x)
                (if (and x (not (consp x)))
                    (stp:append-child element x)
                    element))
              components
              :initial-value
              (make-components))))))

(defun icalendar? ()
  (many1? (named-seq?
           (<- vcal (vcalendar?))
           (many? (crlf?))
           vcal)))

(defun parse-icalendar (str)
  (let ((*default-namespace* *ical-namespace*))
    (make-icalendar-document
     (reduce #'stp:append-child
             (parse-string* (icalendar?) str)
             :initial-value (make-icalendar)))))
