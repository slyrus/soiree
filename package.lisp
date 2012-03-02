
(cl:defpackage :cl-vcard
  (:use :common-lisp :parser-combinators)
  (:nicknames :vcard)
  (:export #:parse-vcard
           #:*vcard-rng-pathname*
           #:*vcard-rng-schema*))

(cl:in-package :cl-vcard)

(defvar *vcard-rng-pathname*
  (merge-pathnames #p"vcard-4_0.rng" cl-vcard-config:*base-directory*))

(defvar *vcard-rng-schema* (cxml-rng:parse-compact *vcard-rng-pathname*))
