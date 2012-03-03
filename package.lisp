
(cl:defpackage :cl-vcard
  (:use :common-lisp :parser-combinators)
  (:nicknames :vcard)
  (:export #:parse-vcard
           #:*vcard-namespace*
           #:*vcard-rng-pathname*
           #:*vcard-rng-schema*))

