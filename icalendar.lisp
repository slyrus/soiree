
(cl:defpackage :soiree-icalendar
  (:use :common-lisp :parser-combinators :soiree :soiree-parse))

(cl:in-package :soiree-icalendar)

(defvar *ical-namespace* "urn:ietf:params:xml:ns:icalendar-2.0")

(defvar *icalendar-rng-pathname*
  (merge-pathnames #p"icalendar-2.0.rnc" soiree-config:*base-directory*))

(defvar *icalendar-rng-schema* (cxml-rng:parse-compact *icalendar-rng-pathname*))

(defun make-date-time-node (element-tag string)
  (add-fset-element-child
   (make-fset-element element-tag *default-namespace*)
   (add-fset-element-child
    (make-fset-element "date-time" *default-namespace*)
    (make-fset-text string))))

(defun date-time-node? (field-name element-tag)
  (named-seq?
   (<- result (content-line? field-name))
   (destructuring-bind (group name params value)
       result
     (make-date-time-node element-tag value))))

(defun dtstamp? () (date-time-node? "DTSTAMP" "dtstamp"))
(defun dtstart? () (date-time-node? "DTSTART" "dtstart"))

(defun class? () (value-text-node? "CLASS" "class"))
(defun created? () (value-text-node? "CREATED" "created"))
(defun description? () (value-text-node? "DESCRIPTION" "class"))
(defun last-mod? () (value-text-node? "LAST-MOD" "last-mod"))
(defun location? () (value-text-node? "LOCATION" "location"))
(defun organizer? () (value-text-node? "ORGANIZER" "organizer"))
(defun priority? () (value-text-node? "PRIORITY" "priority"))
(defun seq? () (value-text-node? "SEQ" "seq"))
(defun status? () (value-text-node? "STATUS" "status"))
(defun transp? () (value-text-node? "TRANSP" "transp"))
(defun recurid? () (value-text-node? "RECURID" "recurid"))



(defun vevent? ()
  (named-seq?
   "BEGIN" ":" "VEVENT" #\Return #\Newline
   (<- content (many? (choices 
                       (dtstamp?)
                       (dtstart?)
                       (uid?)
                       (class?)
                       (created?)
                       (description?)
                       (geo?)
                       (last-mod?)
                       (location?)
                       (organizer?)
                       (priority?)
                       (seq?)
                       (status?)
                       (transp?)
                       (recurid?)
                       (content-line?))))
   "END" ":" "VEVENT" #\Return #\Newline
   (fset:reduce (lambda (element x)
                  (if (and x (not (consp x)))
                      (add-fset-element-child element x)
                      element))
                content
                :initial-value (make-fset-element "vevent" *ical-namespace*))))

(defun vtodo? ()
  (named-seq?
   "BEGIN" ":" "VTODO" #\Return #\Newline
   (<- content (many? (content-line?)))
   "END" ":" "VTODO" #\Return #\Newline
   (fset:reduce (lambda (element x)
                  (if (and x (not (consp x)))
                      (add-fset-element-child element x)
                      element))
                content
                :initial-value (make-fset-element "vtodo" *ical-namespace*))))

(defun vjournal? ()
  (named-seq?
   "BEGIN" ":" "VJOURNAL" #\Return #\Newline
   (<- content (many? (content-line?)))
   "END" ":" "VJOURNAL" #\Return #\Newline
   (fset:reduce (lambda (element x)
                  (if (and x (not (consp x)))
                      (add-fset-element-child element x)
                      element))
                content
                :initial-value (make-fset-element "vjournal" *ical-namespace*))))

(defun vfreebusy? ()
  (named-seq?
   "BEGIN" ":" "VFREEBUSY" #\Return #\Newline
   (<- content (many? (content-line?)))
   "END" ":" "VFREEBUSY" #\Return #\Newline
   (fset:reduce (lambda (element x)
                  (if (and x (not (consp x)))
                      (add-fset-element-child element x)
                      element))
                content
                :initial-value (make-fset-element "vfreebusy" *ical-namespace*))))

(defun vtimezone? ()
(named-seq?
   "BEGIN" ":" "VTIMEZONE" #\Return #\Newline
   (<- content (many? (content-line?)))
   "END" ":" "VTIMEZONE" #\Return #\Newline
   (fset:reduce (lambda (element x)
                  (if (and x (not (consp x)))
                      (add-fset-element-child element x)
                      element))
                content
                :initial-value (make-fset-element "vtimezone" *ical-namespace*))))

(defun component? ()
  (choices (vevent?)
            (vtodo?) 
            (vjournal?)
            (vfreebusy?)
            (vtimezone?)))

(defun calscale? () (value-text-node? "CALSCALE" "calscale"))
(defun method? () (value-text-node? "METHOD" "method"))

(defun calprop? ()
  (choices (prodid?)
           (version?)
           (calscale?)
           (method?)
           (x-name-line?)))

(defun vcalendar? ()
  (named-seq?
   "BEGIN" ":" "VCALENDAR" #\Return #\Newline
   (<- calprops (many? (calprop?)))
   (<- content (many? (component?)))
   "END" ":" "VCALENDAR" #\Return #\Newline
   (fset:reduce (lambda (element x)
             (if (and x (not (consp x)))
                 (add-fset-element-child element x)
                 element))
           content
           :initial-value (make-fset-element "vcalendar" *ical-namespace*))))

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
