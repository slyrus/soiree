
(cl:defpackage :soiree
  (:use :common-lisp :parser-combinators)
  (:nicknames :vcard)
  (:export #:parse-vcard
           #:*vcard-namespace*
           #:*vcard-rng-pathname*
           #:*vcard-rng-schema*))

