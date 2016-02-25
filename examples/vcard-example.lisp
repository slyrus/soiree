
(asdf:load-system 'soiree)

(cl:defpackage #:soiree-example
  (:use #:cl #:soiree))

(cl:in-package #:soiree-example)

(defparameter *baba-oriley-vcard*
  "BEGIN:VCARD
VERSION:3.0
N:O'Riley;Baba;;;
FN:Baba O'Riley
ORG:Polydor Records
TITLE:Field Worker
PHOTO;VALUE=URL;TYPE=GIF:http://www.example.com/dir_photos/my_photo.gif
ADR;type=WORK;type=pref:;;Trafalgar Square;London;England;;UK
TEL;TYPE=WORK,VOICE:(415) 555-1212
TEL;TYPE=HOME,VOICE:(415) 555-1213
EMAIL;TYPE=PREF,INTERNET:thewho@example.com
END:VCARD
")

(defparameter *baba* (parse-vcard *baba-oriley-vcard*))

(defmacro with-vcard-namespace (&body body)
  `(xpath:with-namespaces ((nil soiree::*vcard-namespace*))
     ,@body))

(xpath:string-value
 (with-vcard-namespace
   (xpath:evaluate "/vcards/vcard/fn/text" *baba*)))

(xpath:map-node-set->list
 #'xpath:string-value
 (with-vcard-namespace
   (xpath:evaluate "/vcards/vcard/adr/*/text()" *baba*)))

(stp:serialize *baba* (cxml-rng:make-validator *vcard-rng-schema*))

(stp:string-value
 (stp:find-child
  "text"
  (stp:find-child
   "tel"
   (stp:find-child
    "vcard"
    (stp:find-child 
     "vcards" *baba*
     :key #'stp:local-name :test 'equal)
    :key #'stp:local-name :test 'equal)
   :key #'stp:local-name :test 'equal)
  :key #'stp:local-name :test 'equal))


(xpath:with-namespaces ((nil soiree::*vcard-namespace*))
  (xpath:evaluate "string(/vcards/vcard/tel/*/text())" *baba*))

(xpath:with-namespaces ((nil soiree::*vcard-namespace*))
  (format nil "~A is a ~A who works at ~A and can be reached via e-mail at ~A"
          (xpath:evaluate "string(/vcards/vcard/fn/*/text())" *baba*)
          (xpath:evaluate "string(/vcards/vcard/title/*/text())" *baba*)
          (xpath:evaluate "string(/vcards/vcard/org/*/text())" *baba*)
          (xpath:evaluate "string(/vcards/vcard/email/*/text())" *baba*)))

