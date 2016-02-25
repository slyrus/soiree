(cl:defpackage #:ical-example
  (:use #:cl #:soiree))

(cl:in-package #:ical-example)

(defparameter *test-icalendar*
  (convert-string-to-dos-line-endings "BEGIN:VCALENDAR
PRODID:-//Mozilla.org/NONSGML Mozilla Calendar V1.1//EN
VERSION:2.0
BEGIN:VTIMEZONE
TZID:America/Los_Angeles
BEGIN:DAYLIGHT
TZOFFSETFROM:-0800
TZOFFSETTO:-0700
TZNAME:PDT
DTSTART:19700308T020000
RRULE:FREQ=YEARLY;BYMONTH=3;BYDAY=2SU
END:DAYLIGHT
BEGIN:STANDARD
TZOFFSETFROM:-0700
TZOFFSETTO:-0800
TZNAME:PST
DTSTART:19701101T020000
RRULE:FREQ=YEARLY;BYMONTH=11;BYDAY=1SU
END:STANDARD
END:VTIMEZONE
BEGIN:VEVENT
CREATED:20160225T002930Z
LAST-MODIFIED:20160225T002939Z
DTSTAMP:20160225T002939Z
UID:8e990f09-d084-4542-b08d-14e4da069962
SUMMARY:This is Only a Test
DTSTART;TZID=America/Los_Angeles:20160224T170000
DTEND;TZID=America/Los_Angeles:20160224T180000
TRANSP:OPAQUE
LOCATION:Somewhere
CLASS:PUBLIC
END:VEVENT
END:VCALENDAR
"))

(defparameter *test-event*
  (parse-icalendar *test-icalendar*))

(with-output-to-string (s)
  (stp:serialize *test-event*
                 (cxml:make-character-stream-sink s)))

(defmacro with-ical-namespace (&body body)
  `(xpath:with-namespaces ((nil soiree::*ical-namespace*))
    ,@body))

(defun join-xpath-result (result)
  (if (xpath:node-set-p result)
      (format nil "~{~a~^~&~}"
              (xpath:map-node-set->list #'xpath:string-value result))
      (xpath:string-value result)))

(list
 (join-xpath-result
  (with-ical-namespace
    (xpath:evaluate "/icalendar/vcalendar/components/vevent/properties/summary"
                    *test-event*)))
 (join-xpath-result
  (with-ical-namespace
    (xpath:evaluate "/icalendar/vcalendar/components/vevent/properties/dtstart/date-time"
                    *test-event*)))
 (join-xpath-result
  (with-ical-namespace
    (xpath:evaluate "/icalendar/vcalendar/components/vevent/properties/dtend/date-time"
                    *test-event*))))


(defparameter *test-event-vevent-node-set*
  (with-ical-namespace
    (xpath:evaluate "/icalendar/vcalendar/components/vevent"
                    *test-event*)))


(let ((node (xpath:first-node *test-event-vevent-node-set*)))
  (list
   (join-xpath-result
    (with-ical-namespace
      (xpath:evaluate "properties/summary" node)))
   (join-xpath-result
    (with-ical-namespace
      (xpath:evaluate "properties/dtstart/date-time" node)))
   (join-xpath-result
    (with-ical-namespace
      (xpath:evaluate "properties/dtend/date-time" node)))))

(let ((node (xpath:first-node *test-event-vevent-node-set*)))
  (with-ical-namespace
    (format nil "~A is an event from ~A to ~A"
            (xpath:evaluate "string(properties/summary/text/text())" node)
            (xpath:evaluate "string(properties/dtstart/date-time/text())" node)
            (xpath:evaluate "string(properties/dtend/date-time/text())" node))))

