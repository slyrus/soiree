
(asdf:defsystem :cl-vcard
  :name "cl-vcard"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :licence "BSD"
  :serial t
  :default-component-class cl-source-file
  :depends-on (parser-combinators)
  :components
  ((:static-file "README.md")
   (:static-file "COPYRIGHT")
   (:file "package")
   (:file "vcard")))
