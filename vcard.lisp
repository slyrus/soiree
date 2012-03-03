
(in-package :cl-vcard)

(defvar *vcard-namespace* "urn:ietf:params:xml:ns:vcard-4.0")

(defvar *vcard-rng-pathname*
  (merge-pathnames #p"vcard-4_0.rnc" cl-vcard-config:*base-directory*))

(defvar *vcard-rng-schema* (cxml-rng:parse-compact *vcard-rng-pathname*))

