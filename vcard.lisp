
(in-package :cl-vcard)

(defclass name ()
  ((family-names :accessor family-names :initarg family-names)
   (given-names :accessor given-names :initarg given-names)
   (additional-names :accessor additional-names :initarg additional-names)
   (honorific-prefixes :accessor honorific-prefixes :initarg honorific-prefixes) 
   (honorific-suffixes :accessor honorific-suffixes :initarg honorific-suffixes)))

(defclass vcard ()
  ((formatted-name :accessor vcard-formatted-name :initarg :formatted-name)
   (name :accessor vcard-name :initarg :name)))
