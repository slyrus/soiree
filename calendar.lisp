
(cl:defpackage #:soiree-calendar
  (:use #:common-lisp #:soiree))

(cl:in-package :soiree-calendar)

(defmacro with-ical-namespace (&body body)
  `(xpath:with-namespaces ((nil soiree::*ical-namespace*))
     ,@body))

(defun evaluate (xpath context &optional (unordered-p nil unordered-p-specified))
  (with-ical-namespace
    (apply #'xpath:evaluate xpath context
           (when unordered-p-specified
             (list unordered-p)))))

(defun evaluate1 (xpath context &optional (unordered-p nil unordered-p-specified))
  (let ((node-set (apply #'evaluate xpath context
                         (when unordered-p-specified
                           (list unordered-p)))))
    (when node-set
      (xpath:first-node node-set))))

(defun date-time (node)
  (let ((date-time (evaluate "string(date-time/text())" node)))
    (when (and date-time (plusp (length date-time)))
      date-time)))

(defun date (node)
  (let ((date (evaluate "string(date/text())" node)))
    (when (and date (plusp (length date)))
      date)))

(defun date-time-or-date (node)
  (or (date-time node)
      (date node)))

(defun date-time-or-date-to-universal-time (node)
  (let ((date-time (date-time node)))
    (if date-time
        (soiree-xcal:xcal-date-time-to-universal-time date-time)
        (let ((date (date node)))
          (when date
            (soiree-xcal:xcal-date-to-universal-time date))))))

(defun ical-vevent-summary (ical-node)
  (with-ical-namespace
    (let ((vevent (xpath:first-node
                   (xpath:evaluate "/icalendar/vcalendar/components/vevent" ical-node))))
      (xpath:evaluate "string(properties/summary/text/text())" vevent))))

(defun date-time-or-date-to-timestamp (dtod)
  (local-time:universal-to-timestamp
   (date-time-or-date-to-universal-time dtod)))

(defun ical-vevent-dtstart (ical-node &key (universal t))
  (with-ical-namespace
    (let ((vevent (xpath:first-node
                   (xpath:evaluate "/icalendar/vcalendar/components/vevent" ical-node))))
      (let ((dtstart (evaluate1 "properties/dtstart" vevent)))
        (if universal
            (date-time-or-date-to-universal-time dtstart)
            (date-time-or-date dtstart))))))

(defun ical-vevent-dtend (ical-node &key (universal t))
  (with-ical-namespace
    (let ((vevent (xpath:first-node
                   (xpath:evaluate "/icalendar/vcalendar/components/vevent" ical-node))))
      (let ((dtend (evaluate1 "properties/dtend" vevent)))
        (if universal
            (date-time-or-date-to-universal-time dtend)
            (date-time-or-date dtend))))))

(defun ical-vevent-timezones (ical-node)
  (with-ical-namespace
    (let ((vtimezones (xpath:evaluate "/icalendar/vcalendar/components/vtimezone" ical-node)))
      vtimezones)))

(defun timezone-tzid (tz-node)
  (with-ical-namespace
    (let ((vtimezones (xpath:evaluate "string(properties/tzid/text/text())" tz-node)))
      vtimezones)))

