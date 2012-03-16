
(cl:defpackage :soiree-vcard
  (:use :common-lisp :parser-combinators :soiree :soiree-parse)
  (:shadow #:version? #:geo? #:member #:content-line?))

(cl:in-package :soiree-vcard)

(defvar *vcard-namespace* "urn:ietf:params:xml:ns:vcard-4.0")

(defmacro with-vcard-namespace (&body body)
  `(xpath:with-namespaces ((nil *vcard-namespace*))
     ,@body))

(defvar *vcard-rng-pathname*
  (merge-pathnames #p"vcard-4_0.rnc" soiree-config:*base-directory*))

(defvar *vcard-rng-schema* (cxml-rng:parse-compact *vcard-rng-pathname*))

(defparameter *current-vcard-version* nil)

(defun value-text-node (result)
  (destructuring-bind (group name params value) result
    (declare (ignore group params))
    (apply #'make-fset-value-text-nodes (string-downcase name) (split-string value))))

(defun uri-text-node (result)
  (destructuring-bind (group name params value) result
    (declare (ignore group params))
    (make-fset-uri-text-node (string-downcase name) value)))

;;;
;;; Section 5: Parameters

;; 5.1 language
(defun param-language (params)
  (when-let (language
             (caadar (keep "language" params :test #'string-equal :key #'car)))
    (stp:append-child (stp:make-element "language" *vcard-namespace*)
                      (make-text-node language "language-tag"))))

;; 5.2 pref
(defun param-pref (params)
  (when-let (pref
             (caadar (keep "pref" params :test #'string-equal :key #'car)))
    (stp:append-child (stp:make-element "pref" *vcard-namespace*)
                      (make-text-node pref "integer"))))

;; Note that there is no section 5.3 in the spec!
;; 5.4 altid
(defun param-altid (params)
  (when-let (altid (caadar (keep "altid" params :test #'string-equal :key #'car)))
    (stp:append-child (stp:make-element "altid" *vcard-namespace*)
                      (make-text-node altid))))
;; 5.5 pid
(defun param-pid (params)
  (when-let (pids
             (mapcan #'second (keep "pid" params :test #'string-equal :key #'car)))
    (reduce (lambda (parent pid) (stp:append-child parent (make-text-node pid)))
            pids :initial-value (stp:make-element "pid" *vcard-namespace*))))

;; 5.6 type
(defun param-type (params)
  (when-let (types
             (mapcan #'second (keep "type" params :test #'string-equal :key #'car)))
    (reduce (lambda (parent type)
              (stp:append-child parent (make-text-node type)))
            types :initial-value (stp:make-element "type" *vcard-namespace*))))

;; 5.7 mediatype
(defun param-mediatype (params)
  (when-let (mediatype
             (caadar (keep "mediatype" params :test #'string-equal :key #'car)))
    (stp:append-child (stp:make-element "mediatype" *vcard-namespace*)
                      (make-text-node mediatype "text"))))

;; 5.8 calscale
(defparameter *calscales* '("gregorian"))

(defun param-calscale (params)
  (when-let (calscale
             (caadar (keep "calscale" params :test #'string-equal :key #'car)))
    (when *strict-parsing*
      (unless (cl:member calscale *calscales* :test #'string-equal)
        (error "Unknown calscale: ~A" calscale)))
    (stp:append-child (stp:make-element "calscale" *vcard-namespace*)
                      (make-text-node calscale "text"))))

;; 5.9 sort-as TBD
;; 5.10 geo TBD
;; 5.11 tz TBD

;; parameter utility routines
(defun add-params (param-list params)
  (remove nil (mapcar (lambda (x) (funcall x params)) param-list) :test 'eq))

(defun extract-parameters (params functions)
  (let ((param-element (stp:make-element "parameters" *vcard-namespace*)))
    (let ((param-children (add-params functions params)))
      (reduce #'stp:append-child param-children :initial-value param-element))))

;;;
;;; Section 6: Properties

;; 6.1.3 source
(defun source (result)
  (destructuring-bind (group name params value) result
    (declare (ignore group name))
    (let ((source-node (make-fset-element "source" *vcard-namespace*))
          (param-element (extract-parameters
                          params
                          (list #'param-altid #'param-pid #'param-pref
                                #'param-mediatype))))
      (add-fset-element-child
       (if (plusp (stp:number-of-children param-element))
           (add-fset-element-child source-node param-element)
           source-node)
       (make-fset-text-node "uri" value)))))

;; 6.1.4 kind
(defun kind (result) (value-text-node result))

;; 6.2.1 fn
(defun fn (result)
  (destructuring-bind (group name params value) result
    (declare (ignore group name))
    (let ((fn-node (make-fset-element "fn" *vcard-namespace*))
          (param-element (extract-parameters
                          params
                          (list #'param-language #'param-altid #'param-pid
                                #'param-pref #'param-type))))
      (add-fset-element-child
       (if (plusp (stp:number-of-children param-element))
           (add-fset-element-child fn-node param-element)
           fn-node)
       (make-fset-text-node "text" value)))))

;; 6.2.2 n
(defun n (result)
  (destructuring-bind (group name params value) result
    (declare (ignore group name params))
    (destructuring-bind (family-names
                         given-names
                         additional-names
                         honorific-prefixes
                         honorific-suffixes)
        (split-string value :delimiter #\;)
      (reduce (lambda (parent child)
                (add-fset-element-child parent child))
              (list (apply #'make-fset-text-nodes "surname"
                           (split-string family-names))
                    (apply #'make-fset-text-nodes "given"
                           (split-string given-names))
                    (apply #'make-fset-text-nodes "additional"
                           (split-string additional-names))
                    (apply #'make-fset-text-nodes "prefix"
                           (split-string honorific-prefixes))
                    (apply #'make-fset-text-nodes "suffix"
                           (split-string honorific-suffixes)))
              :initial-value (make-fset-element "n" *vcard-namespace*)))))

;; 6.2.3 nickname
(defun nickname (result)
  (destructuring-bind (group name params value) result
    (declare (ignore group name))
    (let ((nickname-node (make-fset-element "nickname" *vcard-namespace*))
          (param-element (extract-parameters
                          params
                          (list #'param-language #'param-altid #'param-pid
                                #'param-pref #'param-type))))
      (reduce #'add-fset-element-child
              (mapcar #'make-text-node (split-string value))
              :initial-value (if (plusp (stp:number-of-children param-element))
                                 (add-fset-element-child nickname-node param-element)
                                 nickname-node)))))

;; 6.2.4 photo
(defun photo (result)
  (destructuring-bind (group name params value) result
    (declare (ignore group name))
    (let ((photo-node (make-fset-element "photo" *vcard-namespace*))
          (param-element (extract-parameters
                          params
                          (list #'param-altid #'param-pid #'param-pref
                                #'param-type #'param-mediatype))))
      (add-fset-element-child
       (if (plusp (stp:number-of-children param-element))
           (add-fset-element-child photo-node param-element)
           photo-node)
       (make-fset-text-node "uri" value)))))

;; 6.2.5 bday
;; FIXME: we should support the various data elements, instead of just text
(defun bday (result)
  (destructuring-bind (group name params value) result
    (declare (ignore group name))
    (let ((bday-node (make-fset-element "bday" *vcard-namespace*))
          (param-element (extract-parameters
                          params
                          (list #'param-altid #'param-calscale))))
      (add-fset-element-child
       (if (plusp (stp:number-of-children param-element))
           (add-fset-element-child bday-node param-element)
           bday-node)
       (make-fset-text-node "text" value)))))

;; 6.2.6 anniversary
;; FIXME: we should support the various data elements, instead of just text
(defun anniversary (result)
  (destructuring-bind (group name params value) result
    (declare (ignore group name))
    (let ((anniversary-node (make-fset-element "anniversary" *vcard-namespace*))
          (param-element (extract-parameters
                          params
                          (list #'param-altid #'param-calscale))))
      (add-fset-element-child
       (if (plusp (stp:number-of-children param-element))
           (add-fset-element-child anniversary-node param-element)
           anniversary-node)
       (make-fset-text-node "text" value)))))

;; 6.2.7 gender
(defun gender (result) (value-text-node result))

(defun adr (result)
  (destructuring-bind (group name params value) result
    (declare (ignore group name params))
    (destructuring-bind (pobox ext street locality region code country)
        (split-string value :delimiter #\;)
      (reduce (lambda (parent child)
                (add-fset-element-child parent child))
              (list (apply #'make-fset-text-nodes "pobox"
                           (split-string pobox))
                    (apply #'make-fset-text-nodes "ext"
                           (split-string ext))
                    (apply #'make-fset-text-nodes "street"
                           (split-string street))
                    (apply #'make-fset-text-nodes "locality"
                           (split-string locality))
                    (apply #'make-fset-text-nodes "region"
                           (split-string region))
                    (apply #'make-fset-text-nodes "code"
                           (split-string code))
                    (apply #'make-fset-text-nodes "country"
                           (split-string country)))
              :initial-value (make-fset-element "adr" *vcard-namespace*)))))

(defun caladruri (result) (value-text-node result))
(defun caluri (result) (value-text-node result))

;; fix me -- categories needs to accept multiple values
(defun categories (result) (value-text-node result))

(defun clientpidmap (result) (value-text-node result))
(defun email (result) (value-text-node result))

(defun fburl (result) (uri-text-node result))

(defun impp (result) (uri-text-node result))
(defun key (result) (uri-text-node result))

;;; FIXME LANG is broken
(defun lang (result) (value-text-node result))

(defun member (result) (value-text-node result))

(defun note (result) (value-text-node result))
(defun org (result) (value-text-node result))

(defun related (result) (value-text-node result))
(defun rev (result) (value-text-node result))

(defun uid (result) (uri-text-node result))
(defun url (result) (uri-text-node result))

(defun prodid (result) (value-text-node result))

(defun role (result)
  (destructuring-bind (group name params value) result
    (declare (ignore group name))     
    (let ((role-node (make-fset-element "role" *vcard-namespace*))
          (param-element (extract-parameters
                          params
                          (list #'param-language #'param-altid #'param-pid
                                #'param-pref #'param-type))))
      (add-fset-element-child
       (if (plusp (stp:number-of-children param-element))
           (add-fset-element-child role-node param-element)
           role-node)
       (make-fset-text-node "text" value)))))

(defun geo (result)
  (destructuring-bind (group name params value) result
    (declare (ignore group name))
    (let ((geo-node (make-fset-element "geo" *vcard-namespace*))
          (param-element (extract-parameters
                          params
                          (list #'param-altid #'param-pid #'param-pref
                                #'param-type #'param-mediatype))))
      (add-fset-element-child
       (if (plusp (stp:number-of-children param-element))
           (add-fset-element-child geo-node param-element)
           geo-node)
       (make-fset-text-node "uri" value)))))

(defun logo (result)
  (destructuring-bind (group name params value) result
    (declare (ignore group name))
    (let ((logo-node (make-fset-element "logo" *vcard-namespace*))
          (param-element (extract-parameters
                          params
                          (list #'param-language #'param-altid #'param-pid
                                #'param-pref #'param-type #'param-mediatype))))
      (add-fset-element-child
       (if (plusp (stp:number-of-children param-element))
           (add-fset-element-child logo-node param-element)
           logo-node)
       (make-fset-text-node "uri" value)))))

(defun sound (result) (value-text-node result))

(defun make-pref-element (&optional (value 1))
  (stp:append-child
   (stp:make-element "pref" *vcard-namespace*)
   (stp:append-child
    (stp:make-element "integer" *vcard-namespace*)
    (stp:make-text (format nil "~A" value)))))

(defparameter *tel-types* '("work" "home" "text" "voice" "fax"
                            "cell" "video" "pager""textphone"))

(defun tel (result)
  (destructuring-bind (group name params value) result
    (declare (ignore group name))
    (let ((tel-node (make-fset-element "tel" *vcard-namespace*))
          (param-element (stp:make-element "parameters" *vcard-namespace*))
          (types (mapcan #'second
                         (keep "type" params :test #'string-equal :key #'car))))
      (cond ((equal *current-vcard-version* "3.0")
             (when (cl:member "pref" types :test #'string-equal)
               (stp:append-child param-element (make-pref-element)))))
      (let ((tel-types (intersection types *tel-types* :test #'string-equal)))
        (when tel-types
          (let ((type-element (stp:make-element "type" *vcard-namespace*)))
            (reduce
             (lambda (parent child)
               (stp:append-child parent
                                 (make-text-node (string-downcase child))))
             tel-types
             :initial-value type-element)
            (stp:append-child param-element type-element))))
      (add-fset-element-child
       (if (plusp (stp:number-of-children param-element))
           (add-fset-element-child tel-node param-element)
           tel-node)
       (add-fset-element-child
        (make-fset-element "text" *vcard-namespace*)
        (make-fset-text value))))))

(defun title (result)
  (destructuring-bind (group name params value) result
    (declare (ignore group name))
    (let ((title-node (make-fset-element "title" *vcard-namespace*))
          (param-element (extract-parameters
                          params
                          (list #'param-language #'param-altid #'param-pid
                                #'param-pref #'param-type))))
      (add-fset-element-child
       (if (plusp (stp:number-of-children param-element))
           (add-fset-element-child title-node param-element)
           title-node)
       (make-fset-text-node "text" value)))))

(defun tz (result) (value-text-node result))

(defun version (result) 
  (destructuring-bind (group name params value) result
    (declare (ignore group name params))
    (setf *current-vcard-version* value)
    nil))

(defparameter *content-dispatch*
  (let ((hash (make-hash-table :test 'equal)))
    (map nil (lambda (x)
               (setf (gethash (symbol-name x) hash) (symbol-function x)))
         '(adr anniversary bday caladruri caluri categories
           clientpidmap email fburl
           fn geo impp key
           kind lang logo
           member n nickname
           note org photo prodid
           related rev role gender sound source tel title
           tz uid url
           version))
    hash))

(defun handle-content-line (result)
  (destructuring-bind (group name params value) result
       (declare (ignore group params value))
    (let ((fn (gethash (string-upcase name) *content-dispatch*)))
      (when fn (funcall fn result)))))

(defun name-not-end? ()
  (let ((name? (name?)))
    (mdo
      (<- name name?)
      (if (string-equal name "END")
          (zero)
          (result name)))))

(defun content-line? (&optional name)
  ;; [group "."] name *(";" param) ":" value CRLF
  (named-seq?
   (<- group (opt? (hook?
                    #'first
                    (seq-list? (group?) "."))))
   (<- name (if name
                name
                (name-not-end?)))
   (<- params (many? (named-seq?
                      ";"
                      (<- param (param?))
                      param)))
   ":"
   (<- value (soiree-parse::value?))
   (<- long-lines (many? (soiree-parse::long-line-extension?)))
   (seq-list? #\Return #\Newline)
   (when name
     (list group name params (apply #'concatenate 'string value long-lines)))))

(defun vcard? ()
  (named-seq?
   "BEGIN" ":" "VCARD" #\Return #\Newline
   (<- content (many1?
                (content-line?)))
   "END" ":" "VCARD" #\Return #\Newline
   (fset:reduce (lambda (element x)
                  (let ((x (handle-content-line x)))
                    (if (and x (not (consp x)))
                        (add-fset-element-child element x)
                        element)))
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
