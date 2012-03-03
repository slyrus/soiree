
(asdf:defsystem :cl-vcard
  :name "cl-vcard"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :licence "BSD"
  :serial t
  :default-component-class cl-source-file
  :depends-on (parser-combinators cxml-stp cxml-rng fset)
  :components
  ((:static-file "README.md")
   (:static-file "COPYRIGHT")
   (:file "package")
   (:file "utilities")
   (:file "vcard")
   (:file "parse")))

(cl:defpackage #:cl-vcard-config (:export #:*base-directory*))

(cl:defparameter cl-vcard-config:*base-directory* 
  (make-pathname :name nil :type nil :defaults *load-truename*))
