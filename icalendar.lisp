
(in-package :soiree)

(defvar *ical-namespace* "urn:ietf:params:xml:ns:icalendar-2.0")

(defun vcalendar? ()
  (named-seq?
   "BEGIN" ":" "VCALENDAR" #\Return #\Newline


   "END" ":" "VCALENDAR" #\Return #\Newline
   (fset:reduce (lambda (element x)
             (if (and x (not (consp x)))
                 (add-fset-element-child element x)
                 element))
           content
           :initial-value (make-fset-element "vcalendar" *vcard-namespace*))))

(defun parse-icalendar (str)
  (stp:make-document
   (fset:reduce (lambda (element x)
                  (stp:append-child
                   element
                   (unwrap-stp-element x)))
                (parse-string* (many1? (vcalendar?)) str)
                :initial-value (stp:make-element "icalendar" *vcard-namespace*))))
