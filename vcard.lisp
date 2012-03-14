
(cl:defpackage :soiree-vcard
  (:use :common-lisp :parser-combinators :soiree :soiree-parse)
  (:shadow #:version?))

(cl:in-package :soiree-vcard)

(defvar *vcard-namespace* "urn:ietf:params:xml:ns:vcard-4.0")

(defmacro with-vcard-namespace (&body body)
  `(xpath:with-namespaces ((nil *vcard-namespace*))
     ,@body))

(defvar *vcard-rng-pathname*
  (merge-pathnames #p"vcard-4_0.rnc" soiree-config:*base-directory*))

(defvar *vcard-rng-schema* (cxml-rng:parse-compact *vcard-rng-pathname*))

(defparameter *current-vcard-version* nil)

(defun adr? ()
  (named-seq?
   (<- result (content-line? "ADR"))
   (destructuring-bind (group name params value)
       result
     (destructuring-bind (pobox ext street locality region code country)
         (split-string value :delimiter #\;)
       (reduce (lambda (parent child)
                 (add-fset-element-child parent child))
               (list (apply #'make-text-nodes "pobox"
                            (split-string pobox))
                     (apply #'make-text-nodes "ext"
                            (split-string ext))
                     (apply #'make-text-nodes "street"
                            (split-string street))
                     (apply #'make-text-nodes "locality"
                            (split-string locality))
                     (apply #'make-text-nodes "region"
                            (split-string region))
                     (apply #'make-text-nodes "code"
                            (split-string code))
                     (apply #'make-text-nodes "country"
                            (split-string country)))
               :initial-value (make-fset-element "adr" *vcard-namespace*))))))

(defun anniversary? () (value-text-node? "ANNIVERSARY" "anniversary"))
(defun bday? () (value-text-node? "BDAY" "bday"))
(defun caladruri? () (value-text-node? "CALADRURI" "caladruri"))
(defun caluri? () (value-text-node? "CALURI" "caluri"))

;; fix me -- categories needs to accept multiple values
(defun categories? () (value-text-node? "CATEGORIES" "categories"))

(defun clientpidmap? () (value-text-node? "CLIENTPIDMAP" "clientpidmap"))
(defun email? () (value-text-node? "EMAIL" "email"))

(defun fburl? () (uri-text-node? "FBURL" "fburl"))

(defun fn? () (value-text-node? "FN" "fn"))
(defun impp? () (uri-text-node? "IMPP" "impp"))
(defun key? () (uri-text-node? "KEY" "key"))

(defun kind? () (value-text-node? "KIND" "kind"))

;;; FIXME LANG is broken
(defun lang? () (value-text-node? "LANG" "lang"))
(defun logo? () (uri-text-node? "LOGO" "logo"))

(defun member? () (value-text-node? "MEMBER" "member"))
(defun nickname? () (value-text-node? "NICKNAME" "nickname"))

(defun note? () (value-text-node? "NOTE" "note"))
(defun org? () (value-text-node? "ORG" "org"))
(defun photo? () (uri-text-node? "PHOTO" "photo"))

(defun related? () (value-text-node? "RELATED" "related"))
(defun rev? () (value-text-node? "REV" "rev"))

(defun role? () (value-text-node? "ROLE" "role"))
(defun gender? () (value-text-node? "GENDER" "gender"))
(defun sound? () (value-text-node? "SOUND" "sound"))

(defun source? () (value-text-node? "SOURCE" "source"))

(defun tel? ()
  (named-seq?
   (<- result (content-line? "TEL"))
   (destructuring-bind (group name params value)
       result
     (let ((tel-node (make-fset-element "tel" *vcard-namespace*))
           (param-element (stp:make-element "parameters" *vcard-namespace*)))
       (cond ((equal *current-vcard-version* "3.0")
              (let ((types (mapcan #'second
                                   (keep "type" params :test #'equal :key #'car))))
                (cond ((member "pref" types :test #'string-equal)
                       (stp:append-child
                        param-element
                        (stp:append-child
                         (stp:make-element "pref" *vcard-namespace*)
                         (stp:append-child
                          (stp:make-element "integer" *vcard-namespace*)
                          (stp:make-text "1")))))))))
       (add-fset-element-child
        (if (plusp (stp:number-of-children param-element))
            (add-fset-element-child tel-node param-element)
            tel-node)
        (add-fset-element-child
         (make-fset-element "text" *vcard-namespace*)
         (make-fset-text value)))))))

(defun title? () (value-text-node? "TITLE" "title"))

(defun tz? () (value-text-node? "TZ" "tz"))

(defun n? ()
  (named-seq?
   (<- result (content-line? "N"))
   (destructuring-bind (group name params value)
       result
     (destructuring-bind (family-names
                          given-names
                          additional-names
                          honorific-prefixes
                          honorific-suffixes)
         (split-string value :delimiter #\;)
       (reduce (lambda (parent child)
                 (add-fset-element-child parent child))
               (list (apply #'make-text-nodes "surname"
                            (split-string family-names))
                     (apply #'make-text-nodes "given"
                            (split-string given-names))
                     (apply #'make-text-nodes "additional"
                            (split-string additional-names))
                     (apply #'make-text-nodes "prefix"
                            (split-string honorific-prefixes))
                     (apply #'make-text-nodes "suffix"
                            (split-string honorific-suffixes)))
               :initial-value (make-fset-element "n" *vcard-namespace*))))))

(defun version? () 
  (named-seq?
   (<- result (content-line? "VERSION"))
   (destructuring-bind (group name params value)
       result
     (setf *current-vcard-version* value)
     nil)))

(defun vcard? ()
  (named-seq?
   "BEGIN" ":" "VCARD" #\Return #\Newline
   (<- content (many1?
                (choices1
                 (adr?)
                 (anniversary?)
                 (bday?)
                 (caladruri?)
                 (caluri?)
                 (categories?)

                 (clientpidmap?) (email?) (fburl?)

                 (fn?) (geo?) (impp?) (key?)
                 
                 (kind?) (lang?) (logo?)

                 (member?) (n?) (nickname?)

                 (note?) (org?) (photo?)

                 (prodid?) (related?) (rev?)

                 (role?) (gender?) (sound?)

                 (source?) (tel?) (title?)

                 (tz?) (uid?) (url?)

                 (x-name-line?)
                 
                 (version?))))

   "END" ":" "VCARD" #\Return #\Newline
   (fset:reduce (lambda (element x)
                  (if (and x (not (consp x)))
                      (add-fset-element-child element x)
                      element))
                content
                :initial-value (make-fset-element "vcard" *vcard-namespace*))))

(defun parse-vcard (str)
  (let ((*default-namespace* *vcard-namespace*)
        (*current-vcard-version* nil))
    (stp:make-document
     (fset:reduce (lambda (element x)
                    (stp:append-child
                     element
                     (unwrap-stp-element x)))
                  (parse-string* (many1? (vcard?)) str)
                  :initial-value
                  (let ((element (stp:make-element "vcards" *vcard-namespace*)))
                    (cxml-stp:add-extra-namespace element "" *vcard-namespace*)
                    element)))))
