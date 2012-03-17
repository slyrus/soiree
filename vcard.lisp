
(cl:defpackage :soiree-vcard
  (:use :common-lisp :parser-combinators :soiree :soiree-parse)
  (:shadow #:version #:geo #:member))

(cl:in-package :soiree-vcard)

(defvar *vcard-namespace* "urn:ietf:params:xml:ns:vcard-4.0")

(defmacro with-vcard-namespace (&body body)
  `(xpath:with-namespaces ((nil *vcard-namespace*))
     ,@body))

(defvar *vcard-rng-pathname*
  (merge-pathnames #p"vcard-4_0.rnc" soiree-config:*base-directory*))

(defvar *vcard-rng-schema* (cxml-rng:parse-compact *vcard-rng-pathname*))

(defparameter *current-vcard-version* nil)

(defun make-pref-element (&optional (value 1))
  (stp:append-child
   (stp:make-element "pref" *vcard-namespace*)
   (stp:append-child
    (stp:make-element "integer" *vcard-namespace*)
    (stp:make-text (format nil "~A" value)))))

;;;
;;; Section 5: Parameters

;; 5.1 language
(defun param-language (params)
  (when-let (language
             (caadar (keep "language" params :test #'string-equal :key #'car)))
    (stp:append-child (stp:make-element "language" *vcard-namespace*)
                      (make-text-node "language-tag" language))))

;; 5.2 pref
(defun param-pref (params)
  (when-let (pref
             (caadar (keep "pref" params :test #'string-equal :key #'car)))
    (stp:append-child (stp:make-element "pref" *vcard-namespace*)
                      (make-text-node "integer" pref))))

;; Note that there is no section 5.3 in the spec!
;; 5.4 altid
(defun param-altid (params)
  (when-let (altid (caadar (keep "altid" params :test #'string-equal :key #'car)))
    (stp:append-child (stp:make-element "altid" *vcard-namespace*)
                      (make-text-node "text" altid))))
;; 5.5 pid
(defun param-pid (params)
  (when-let (pids
             (mapcan #'second (keep "pid" params :test #'string-equal :key #'car)))
    (reduce (lambda (parent pid)
              (stp:append-child parent
                                (make-text-node "text" pid)))
            pids :initial-value (stp:make-element "pid" *vcard-namespace*))))

;; 5.6 type
(defun param-type (params)
  (when-let (types
             (mapcan #'second (keep "type" params :test #'string-equal :key #'car)))
    (reduce (lambda (parent type)
              (stp:append-child parent
                                (make-text-node "text" type)))
            types :initial-value (stp:make-element "type" *vcard-namespace*))))

;; 5.7 mediatype
(defun param-mediatype (params)
  (when-let (mediatype
             (caadar (keep "mediatype" params :test #'string-equal :key #'car)))
    (stp:append-child (stp:make-element "mediatype" *vcard-namespace*)
                      (make-text-node "text" mediatype))))

;; 5.8 calscale
(defparameter *calscales* '("gregorian"))

(defun param-calscale (params)
  (when-let (calscale
             (caadar (keep "calscale" params :test #'string-equal :key #'car)))
    (when *strict-parsing*
      (unless (cl:member calscale *calscales* :test #'string-equal)
        (error "Unknown calscale: ~A" calscale)))
    (stp:append-child (stp:make-element "calscale" *vcard-namespace*)
                      (make-text-node "text" calscale))))

;; 5.9 sort-as TBD
(defun param-sort-as (params)
  (when-let (sort-as (caadar (keep "sort-as" params
                                   :test #'string-equal :key #'car)))
    (reduce #'stp:append-child
            (apply #'make-text-node-list "text" (split-string sort-as))
            :initial-value (stp:make-element "sort-as" *vcard-namespace*))))

;; 5.10 geo
(defun param-geo (params)
  (when-let (geo (caadar (keep "geo" params :test #'string-equal :key #'car)))
    (stp:append-child (stp:make-element "geo" *vcard-namespace*)
                      (make-text-node "uri" geo))))

;; 5.11 tz
(defun param-tz (params)
  (when-let (tz (caadar (keep "tz" params :test #'string-equal :key #'car)))
    (stp:append-child (stp:make-element "tz" *vcard-namespace*)
                      (make-text-node "uri" tz))))

;; parameter utility routines
(defun add-params (param-list params)
  (remove nil (mapcar (lambda (x) (funcall x params)) param-list) :test 'eq))

(defun extract-parameters (params functions)
  (let ((param-element (stp:make-element "parameters" *vcard-namespace*)))
    (let ((param-children (add-params functions params)))
      (reduce #'stp:append-child param-children :initial-value param-element))))

;;;
;;; Section 6: Properties

(defmacro def-generic-property (property-name element-name
                                parameter-functions value-node-type
                                &key multiple-values)
  `(defun ,property-name (result)
     (destructuring-bind (group name params value) result
       (declare (ignore group name))
       (let ((node (stp:make-element ,element-name *vcard-namespace*))
             (param-element (extract-parameters
                             params
                             ,parameter-functions)))
         (when (plusp (stp:number-of-children param-element))
           (stp:append-child node param-element))
         ,(if multiple-values
              `(reduce #'stp:append-child
                       (apply #'make-text-node-list ,value-node-type
                              (split-string value))
                       :initial-value node)
              `(stp:append-child
                node
                (make-text-node ,value-node-type value)))))))

;; 6.1.3 source
(def-generic-property source "source"
  (list #'param-altid #'param-pid #'param-pref #'param-mediatype)
  "uri")

;; 6.1.4 kind
(def-generic-property kind "kind" nil "uri")

;; 6.2.1 fn
(def-generic-property fn "fn"
  (list #'param-language #'param-altid #'param-pid #'param-pref #'param-type)
  "text")

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
                (stp:append-child parent child))
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
              :initial-value (stp:make-element "n" *vcard-namespace*)))))

;; 6.2.3 nickname
(def-generic-property nickname "nickname"
  (list #'param-language #'param-altid #'param-pid #'param-pref #'param-type)
  "text" :multiple-values t)

;; 6.2.4 photo
(def-generic-property photo "photo"
  (list #'param-altid #'param-pid #'param-pref #'param-type #'param-mediatype)
  "uri")

;; 6.2.5 bday
;; FIXME: we should support the various data elements, instead of just text
(defun bday (result)
  (destructuring-bind (group name params value) result
    (declare (ignore group name))
    (let ((bday-node (stp:make-element "bday" *vcard-namespace*))
          (param-element (extract-parameters
                          params
                          (list #'param-altid #'param-calscale))))
      (stp:append-child
       (if (plusp (stp:number-of-children param-element))
           (stp:append-child bday-node param-element)
           bday-node)
       (make-text-node "text" value)))))

;; 6.2.6 anniversary
;; FIXME: we should support the various data elements, instead of just text
(defun anniversary (result)
  (destructuring-bind (group name params value) result
    (declare (ignore group name))
    (let ((anniversary-node (stp:make-element "anniversary" *vcard-namespace*))
          (param-element (extract-parameters
                          params
                          (list #'param-altid #'param-calscale))))
      (stp:append-child
       (if (plusp (stp:number-of-children param-element))
           (stp:append-child anniversary-node param-element)
           anniversary-node)
       (make-text-node "text" value)))))

;; 6.2.7 gender
(defun gender (result)
  (destructuring-bind (group name params value) result
    (declare (ignore group name params))
    (destructuring-bind (sex &optional identity)
        (split-string value :delimiter #\;)
      (let ((gender-element (stp:make-element "gender" *default-namespace*)))
        (when sex
          (stp:append-child
           gender-element
           (make-text-node "sex" sex)))
        (when identity
          (stp:append-child
           gender-element
           (make-text-node "identity" identity)))
        gender-element))))

;; 6.3.1 adr
(defun adr (result)
  (destructuring-bind (group name params value) result
    (declare (ignore group name))
    (destructuring-bind (pobox ext street locality region code country)
        (split-string value :delimiter #\;)
      (let ((adr-node (stp:make-element "adr" *vcard-namespace*))
            (param-element (extract-parameters
                            params
                            (list #'param-language #'param-altid #'param-pid
                                  #'param-pref #'param-type #'param-geo #'param-tz
                                  ;; FIXME add param-label!
                                  ))))
        (if (plusp (stp:number-of-children param-element))
            (stp:append-child adr-node param-element)
            adr-node)
        (reduce (lambda (parent child)
                  (stp:append-child parent child))
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
                :initial-value adr-node)))))

;; 6.4.1 tel
(defparameter *tel-types* '("work" "home" "text" "voice" "fax"
                            "cell" "video" "pager" "textphone"))

(defun tel (result)
  (destructuring-bind (group name params value) result
    (declare (ignore group name))
    (let ((tel-node (stp:make-element "tel" *vcard-namespace*))
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
                                 (make-text-node "text" (string-downcase child))))
             tel-types
             :initial-value type-element)
            (stp:append-child param-element type-element))))
      (stp:append-child
       (if (plusp (stp:number-of-children param-element))
           (stp:append-child tel-node param-element)
           tel-node)
       (stp:append-child
        (stp:make-element "text" *vcard-namespace*)
        (stp:make-text value))))))

;; 6.4.2 email
(def-generic-property email "email"
  (list #'param-altid #'param-pid #'param-pref #'param-type)
  "text")

;; 6.4.3 impp
(def-generic-property impp "impp"
  (list #'param-altid #'param-pid #'param-pref #'param-type #'param-mediatype)
  "uri")

;; 6.4.4 lang
(def-generic-property lang "lang"
  (list #'param-altid #'param-pid #'param-pref #'param-type)
  "language-tag")

;; 6.5.1 tz
(def-generic-property tz "tz"
  (list #'param-altid #'param-pid #'param-pref #'param-type #'param-mediatype)
  "uri")

;; 6.5.2 geo
(def-generic-property geo "geo"
  (list #'param-altid #'param-pid #'param-pref #'param-type #'param-mediatype)
  "uri")

;; 6.6.1 title
(def-generic-property title "title"
  (list #'param-language #'param-altid #'param-pid #'param-pref #'param-type)
  "text")

;; 6.6.2 role
(def-generic-property role "role"
  (list #'param-language #'param-altid #'param-pid #'param-pref #'param-type)
  "text")

;; 6.6.3 logo
(def-generic-property logo "logo"
  (list #'param-language #'param-altid #'param-pid #'param-pref #'param-type
        #'param-mediatype)
  "uri")

;; 6.6.4 org
(def-generic-property org "org"
  (list #'param-language #'param-altid #'param-pid #'param-pref #'param-type
        #'param-sort-as)
  "text" :multiple-values t)

;; 6.6.5 member
(def-generic-property member "member"
  (list #'param-altid #'param-pid #'param-pref #'param-mediatype)
  "uri")

;; 6.6.6 related
;; FIXME this is broken
(defun related (result) (text-content result))

;; 6.7.1 categories
(def-generic-property categories "categories"
  (list #'param-language #'param-altid #'param-pid #'param-pref #'param-type)
  "text" :multiple-values t)

;; 6.7.2 note
(def-generic-property note "note"
  (list #'param-language #'param-altid #'param-pid #'param-pref #'param-type)
  "text")

;; 6.7.3 prodid
(def-generic-property prodid "prodid" nil "text")

;; 6.7.4 rev
(def-generic-property rev "rev" nil "time")

;; 6.7.5 sound
(def-generic-property sound "sound"
  (list #'param-altid #'param-pid #'param-pref #'param-type #'param-mediatype)
  "uri")

;; 6.7.6 uid
(def-generic-property uid "uid" nil "uri")

;; 6.7.7 clientpidmap
;; FIXME this needs to support the source id element!
(def-generic-property clientpidmap "clientpidmap" nil "uri")

;; 6.7.8 url
(def-generic-property url "url"
  (list #'param-altid #'param-pid #'param-pref #'param-type #'param-mediatype)
  "uri")

;; 6.8.1 key
;; FIXME we should support either value-key or value-uri
(def-generic-property key "key"
  (list #'param-altid #'param-pid #'param-pref #'param-type #'param-mediatype)
  "uri")

;; 6.9.1 fburl
(def-generic-property fburl "fburl"
  (list #'param-altid #'param-pid #'param-pref #'param-type #'param-mediatype)
  "uri")

;; 6.9.2 caladruri
(def-generic-property caladruri "caladruri"
  (list #'param-altid #'param-pid #'param-pref #'param-type #'param-mediatype)
  "uri")

;; 6.9.3 caluri
(def-generic-property caluri "caluri"
  (list #'param-altid #'param-pid #'param-pref #'param-type #'param-mediatype)
  "uri")

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

(defun vcard? ()
  (named-seq?
   "BEGIN" ":" "VCARD" #\Return #\Newline
   (<- content (many1?
                (content-line?)))
   "END" ":" "VCARD" #\Return #\Newline
   (reduce (lambda (element x)
             (let ((x (handle-content-line x)))
               (if (and x (not (consp x)))
                   (stp:append-child element x)
                   element)))
           content
           :initial-value (stp:make-element "vcard" *vcard-namespace*))))

(defun parse-vcard (str)
  (let ((*default-namespace* *vcard-namespace*)
        (*current-vcard-version* nil))
    (stp:make-document
     (reduce #'stp:append-child
             (parse-string* (many1? (vcard?)) str)
             :initial-value
             (let ((element (stp:make-element "vcards" *vcard-namespace*)))
               (cxml-stp:add-extra-namespace element "" *vcard-namespace*)
               element)))))
