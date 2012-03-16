
(cl:defpackage :soiree
  (:use :common-lisp :parser-combinators)
  (:nicknames :vcard)
  (:export #:parse-vcard
           #:*vcard-namespace*
           #:*vcard-rng-pathname*
           #:*vcard-rng-schema*

           #:parse-icalendar
           #:*ical-namespace*
           #:*ical-rng-pathname*
           #:*ical-rng-schema*

           #:contents-of-stream
           #:contents-of-file

           #:remove-keyword
           #:keep
           #:when-let))

