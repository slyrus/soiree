
(cl:defpackage :soiree-parse
  (:use :common-lisp :parser-combinators :soiree)
  (:export #:*strict-parsing*
           #:*default-namespace*

           #:make-text-nodes

           #:text-content
           #:uri-content

           #:crlf?
           #:qsafe-char?
           #:safe-char?
           #:param-values?
           #:param?
           #:group?

           #:split-string

           #:name?

           #:geo
           #:uid
           #:url
           #:version
           #:prodid
           
           #:x-name-line?
           #:content-line?))

(cl:in-package :soiree-parse)

(defparameter *default-namespace* nil)

(defparameter *strict-parsing* t)

(defun make-text-nodes (element-tag &rest strings)
  (reduce (lambda (element x)
            (stp:append-child
             element
             (stp:make-text x)))
          strings
          :initial-value (stp:make-element element-tag *default-namespace*)))

(def-cached-parser crlf?
  (seq-list? #\Return #\Newline))

(defun non-ascii-p (char)
  (<= #x80 (char-code char) #xff))

(def-cached-parser non-ascii?
  (sat #'non-ascii-p))

(defun wsp-p (char)
  (or (eql char #\space)
      (eql char #\tab)))

(def-cached-parser wsp?
  (sat #'wsp-p))

(defun qsafe-char-p (char)
  (let ((code (char-code char)))
    (or (wsp-p char)
        (= code #x21)
        (<= #x23 code #x7e)
        (<= #x80 code #xff))))

(def-cached-parser qsafe-char?
  (sat #'qsafe-char-p))

(defun safe-char-p (char)
  (let ((code (char-code char)))
    (or (wsp-p char)
        (= code #x21)
        (<= #x23 code #x39)
        (<= #x3c code #x7e)
        (<= #x80 code #xff))))

(def-cached-parser safe-char?
  (sat #'safe-char-p))

(defun vcharp (char)
  (<= #x21 (char-code char) #x7e))

(def-cached-parser vchar?
  (sat #'vcharp))

(defun value-char-p (char)
  (let ((code (char-code char)))
    (or (wsp-p char)
        (<= #x21 code #x7e)
        (<= #x80 code #xff))))

(def-cached-parser value-char?
  (sat #'value-char-p))

(defun param-values? ()
  (hook? #'split-string
         (param-value?)))

(defun param? ()
  (named-seq?
   (<- param-name (param-name?))
   #\=
   (<- param-values (param-values?))
   (list param-name param-values)))

(def-cached-parser value?
  (between? (choices (vchar?) (wsp?) (non-ascii?))
            1 nil 'string))

(def-cached-parser alphanum-or-dash?
  (choice1
   #\-
   (sat #'alphanumericp)))

(def-cached-parser group?
  (between? (alphanum?) 1 nil 'string))

(def-cached-parser param-name?
  (between? (choice1 (alphanum?) #\-) 1 nil 'string))

(defun param-value? ()
  (choice1
   (between? (safe-char?) 1 nil 'string)
   (named-seq?
    #\"
    (<- quoted-value (between? (qsafe-char?) 1 nil 'string))
    #\"
    quoted-value)))

(defun escaped-string? (char &optional (escape-char #\\))
  (many?
   (choice1
    (named-seq? escape-char (<- c (item)) c)
    char)))

(defun split-string (str &key (escape-char #\\) (delimiter #\,))
  (let (escaped acc cur)
    (loop for c across str
          do (if escaped
                 (progn
                   (cond ((char-equal c #\n) (push #\newline cur))
                         (t (push c cur)))
                   (setf escaped nil))
                 (progn
                   (cond ((eql c delimiter)
                          (push (nreverse (coerce cur 'string)) acc)
                          (setf cur nil))
                         ((eql c escape-char)
                          (setf escaped t))
                         (t (push c cur)))))
          finally
             (push (nreverse (coerce cur 'string)) acc)
             (return (reverse acc)))))

(defun name? ()
  (between? (alphanum-or-dash?) 1 nil 'string))

(defun x-name? ()
  (named-seq?
   (<- x (choice #\X #\x))
   #\-
   (<- name (between? (alphanum-or-dash?) 1 nil 'string))
   (concatenate 'string (string x) "-" name)))

(defun text-content (result)
  (destructuring-bind (group name params value) result
    (declare (ignore group params))
    (reduce (lambda (element x)
              (stp:append-child
               element
               (stp:append-child
                (stp:make-element "text" *default-namespace*)
                (stp:make-text x))))
            (split-string value)
            :initial-value (stp:make-element (string-downcase name) *default-namespace*))))

(defun uri-content (result)
  (destructuring-bind (group name params value) result
    (declare (ignore group params))
    (stp:append-child
     (stp:make-element (string-downcase name) *default-namespace*)
     (stp:append-child
      (stp:make-element "uri" *default-namespace*)
      (stp:make-text value)))))

(defun geo (result) (uri-content result))

(defun uid (result) (uri-content result))
(defun url (result) (uri-content result))

(defun version (result) nil)
(defun prodid (result) (text-content result))

(defun long-line-extension? ()
  (named-seq?
   #\Return
   #\Newline
   (wsp?)
   (<- value (value?))
   value))

(defun x-name-line? ()
  ;; [group "."] name *(";" param) ":" value CRLF
  (named-seq?
   (<- group (opt? (hook?
                    #'first
                    (seq-list? (group?) "."))))
   (<- name (x-name?))
   (<- params (many? (named-seq?
                      ";"
                      (<- param (param?))
                      param)))
   ":"
   (<- value (value?))
   (<- long-lines (many? (long-line-extension?)))
   (seq-list? #\Return #\Newline)
   (when name
     (list group name params (apply #'concatenate 'string value long-lines)))))

(defun name-not-end? ()
  (let ((name? (name?)))
    (mdo
      (<- name name?)
      (if (string-equal name "END")
          (zero)
          (result name)))))

(defun content-line? ()
  ;; [group "."] name *(";" param) ":" value CRLF
  (named-seq?
   (<- group (opt? (hook?
                    #'first
                    (seq-list? (group?) "."))))
   (<- name (name-not-end?))
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

