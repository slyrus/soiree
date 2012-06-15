
(asdf:defsystem :soiree-dav
  :name "soiree"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :licence "BSD"
  :serial t
  :default-component-class cl-source-file
  :depends-on (soiree drakma cxml cxml-stp xpath)
  :components
  ((:file "dav")
   (:file "carddav")))
