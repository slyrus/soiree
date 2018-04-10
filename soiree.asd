
(asdf:defsystem :soiree
  :name "soiree"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :licence "BSD"
  :serial t
  :default-component-class cl-source-file
  :depends-on (parser-combinators cxml-stp cxml-rng cl-base64 local-time)
  :components
  ((:static-file "README.md")
   (:static-file "COPYRIGHT")
   (:file "package")
   (:file "utilities")
   (:file "parse")
   (:file "vcard")
   (:file "icalendar")
   (:file "xcal")
   (:file "time")))

(cl:defpackage #:soiree-config (:export #:*base-directory*))

(cl:defparameter soiree-config:*base-directory* 
  (make-pathname :name nil :type nil :defaults *load-truename*))
