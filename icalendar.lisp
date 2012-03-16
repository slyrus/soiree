
(cl:defpackage :soiree-icalendar
  (:use :common-lisp :parser-combinators :soiree :soiree-parse)
  (:shadow #:class #:method))

(cl:in-package :soiree-icalendar)

(defvar *ical-namespace* "urn:ietf:params:xml:ns:icalendar-2.0")

(defvar *ical-rng-pathname*
  (merge-pathnames #p"icalendar-2.0.rnc" soiree-config:*base-directory*))

(defvar *ical-rng-schema* (cxml-rng:parse-compact *icalendar-rng-pathname*))

(defun make-fset-date-time-node (element-tag string)
  (add-fset-element-child
   (make-fset-element (string-downcase element-tag) *ical-namespace*)
   (add-fset-element-child
    (make-fset-element "date-time" *ical-namespace*)
    (make-fset-text string))))

(defun date-time-node (result)
  (destructuring-bind (group name params value) result
    (declare (ignore group params))
    (make-fset-date-time-node name value)))

;;; FIXME!! dtstamp and dtstart (and friends?) need to convert from
;;; icalendar style dates/times to xcal dates and times
;;;
(defun dtstamp (result) (date-time-node result))
(defun dtstart (result) (date-time-node result))

(defun make-fset-cal-address-node (element-tag string)
  (add-fset-element-child
   (make-fset-element (string-downcase element-tag) *ical-namespace*)
   (add-fset-element-child
    (make-fset-element "cal-address" *ical-namespace*)
    (make-fset-text string))))

(defun cal-address-node (result)
  (destructuring-bind (group name params value) result
    (declare (ignore group params))
    (make-fset-cal-address-node (string-downcase name) value)))

(defun attendee (result) (cal-address-node result))
(defun class (result) (value-text-node result))
(defun created (result) (value-text-node result))
(defun description (result) (value-text-node result))
(defun last-mod (result) (value-text-node result))
(defun location (result) (value-text-node result))

(defun organizer (result)
  (destructuring-bind (group name params value) result
    (declare (ignore group params))
    (make-fset-cal-address-node name value)))

(defun priority (result) (value-text-node result))
(defun seq (result) (value-text-node result))
(defun status (result) (value-text-node result))
(defun transp (result) (value-text-node result))
(defun recurid (result) (value-text-node result))

(defun vevent? ()
  (named-seq?
   "BEGIN" ":" "VEVENT" #\Return #\Newline
   (<- content (many? (content-line?)))
   "END" ":" "VEVENT" #\Return #\Newline
   (add-fset-element-child
    (make-fset-element "vevent" *ical-namespace*)
    (fset:reduce (lambda (element x)
                   (let ((x (handle-content-line x)))
                     (if (and x (not (consp x)))
                         (add-fset-element-child element x)
                         element)))
                 content
                 :initial-value (make-fset-element "properties" *ical-namespace*)))))

(defun vtodo? ()
  (named-seq?
   "BEGIN" ":" "VTODO" #\Return #\Newline
   (<- content (many? (content-line?)))
   "END" ":" "VTODO" #\Return #\Newline
   (fset:reduce (lambda (element x)
                  (let ((x (handle-content-line x)))
                    (if (and x (not (consp x)))
                        (add-fset-element-child element x)
                        element)))
                content
                :initial-value (make-fset-element "vtodo" *ical-namespace*))))

(defun vjournal? ()
  (named-seq?
   "BEGIN" ":" "VJOURNAL" #\Return #\Newline
   (<- content (many? (content-line?)))
   "END" ":" "VJOURNAL" #\Return #\Newline
   (fset:reduce (lambda (element x)
                  (let ((x (handle-content-line x)))
                    (if (and x (not (consp x)))
                        (add-fset-element-child element x)
                        element)))
                content
                :initial-value (make-fset-element "vjournal" *ical-namespace*))))

(defun vfreebusy? ()
  (named-seq?
   "BEGIN" ":" "VFREEBUSY" #\Return #\Newline
   (<- content (many? (content-line?)))
   "END" ":" "VFREEBUSY" #\Return #\Newline
   (fset:reduce (lambda (element x)
                  (let ((x (handle-content-line x)))
                    (if (and x (not (consp x)))
                        (add-fset-element-child element x)
                        element)))
                content
                :initial-value (make-fset-element "vfreebusy" *ical-namespace*))))

(defun vtimezone? ()
(named-seq?
   "BEGIN" ":" "VTIMEZONE" #\Return #\Newline
   (<- content (many? (content-line?)))
   "END" ":" "VTIMEZONE" #\Return #\Newline
   (fset:reduce (lambda (element x)
                  (let ((x (handle-content-line x)))
                    (if (and x (not (consp x)))
                        (add-fset-element-child element x)
                        element)))
                content
                :initial-value (make-fset-element "vtimezone" *ical-namespace*))))

(defun component? ()
  (choices (vevent?)
           (vtodo?) 
           (vjournal?)
           (vfreebusy?)
           (vtimezone?)))

(defun calscale (result) (value-text-node result))
(defun method (result) (value-text-node result))

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
   
   (add-fset-element-child
    (add-fset-element-child
     (make-fset-element "vcalendar" *ical-namespace*)
     (add-fset-element-child
      (add-fset-element-child
       (make-fset-element "properties" *ical-namespace*)
       (add-fset-element-child
        (make-fset-element "version" *ical-namespace*)
        (make-text-node "2.0")))
      (add-fset-element-child
       (make-fset-element "prodid" *ical-namespace*)
       (make-text-node "bar"))))
    (fset:reduce (lambda (element x)
                  (if (and x (not (consp x)))
                      (add-fset-element-child element x)
                      element))
                content
                :initial-value
                (make-fset-element "components" *ical-namespace*)))))

(defun icalendar? ()
  (many1? (named-seq?
           (<- vcal (vcalendar?))
           (many? (crlf?))
           vcal)))

(defun parse-icalendar (str)
  (let ((*default-namespace* *ical-namespace*))
    (stp:make-document
     (fset:reduce (lambda (element x)
                    (stp:append-child
                     element
                     (unwrap-stp-element x)))
                  (parse-string* (icalendar?) str)
                  :initial-value (stp:make-element "icalendar" *ical-namespace*)))))
