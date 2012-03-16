
(cl:defpackage :soiree-icalendar
  (:use :common-lisp :parser-combinators :soiree :soiree-parse)
  (:shadow #:class #:method))

(cl:in-package :soiree-icalendar)

(defvar *ical-namespace* "urn:ietf:params:xml:ns:icalendar-2.0")

(defvar *ical-rng-pathname*
  (merge-pathnames #p"icalendar-2.0.rnc" soiree-config:*base-directory*))

(defvar *ical-rng-schema* (cxml-rng:parse-compact *ical-rng-pathname*))

(defun make-date-time-node (element-tag string)
  (stp:append-child
   (stp:make-element (string-downcase element-tag) *ical-namespace*)
   (stp:append-child
    (stp:make-element "date-time" *ical-namespace*)
    (stp:make-text string))))

(defun date-time-node (result)
  (destructuring-bind (group name params value) result
    (declare (ignore group params))
    (make-date-time-node name value)))

;;; FIXME!! dtstamp and dtstart (and friends?) need to convert from
;;; icalendar style dates/times to xcal dates and times
;;;
(defun dtstamp (result) (date-time-node result))
(defun dtstart (result) (date-time-node result))

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
(defun class (result) (make-element-with-text* result))
(defun created (result) (make-element-with-text* result))
(defun description (result) (make-element-with-text* result))
(defun last-mod (result) (make-element-with-text* result))
(defun location (result) (make-element-with-text* result))

(defun organizer (result)
  (destructuring-bind (group name params value) result
    (declare (ignore group params))
    (make-cal-address-node name value)))

(defun priority (result) (make-element-with-text* result))
(defun seq (result) (make-element-with-text* result))
(defun status (result) (make-element-with-text* result))
(defun transp (result) (make-element-with-text* result))
(defun recurid (result) (make-element-with-text* result))

(defun vevent? ()
  (named-seq?
   "BEGIN" ":" "VEVENT" #\Return #\Newline
   (<- content (many? (content-line?)))
   "END" ":" "VEVENT" #\Return #\Newline
   (stp:append-child
    (stp:make-element "vevent" *ical-namespace*)
    (reduce (lambda (element x)
              (let ((x (handle-content-line x)))
                (if (and x (not (consp x)))
                    (stp:append-child element x)
                    element)))
            content
            :initial-value (stp:make-element "properties" *ical-namespace*)))))

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

(defun calscale (result) (make-element-with-text* result))
(defun method (result) (make-element-with-text* result))

(defun calprop? ()
  (content-line?))

(defparameter *content-dispatch*
  (let ((hash (make-hash-table :test 'equal)))
    (map nil (lambda (x)
               (setf (gethash (symbol-name x) hash) (symbol-function x)))
         '(dtstamp dtstart attendee class created description last-mod
           location organizer priority seq status transp recurid))
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
        (make-element-with-text "text" "2.0")))
      (stp:append-child
       (stp:make-element "prodid" *ical-namespace*)
       (make-element-with-text "text" "bar"))))
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
