
(cl:defpackage :soiree-parse
  (:use :common-lisp :parser-combinators :soiree)
  (:export #:*default-namespace*

           #:make-text-node

           #:wrap-stp-element
           #:unwrap-stp-element
           #:make-fset-element
           #:add-fset-element-child
           #:make-fset-text
           #:make-fset-text-node
           #:make-fset-text-nodes
           #:make-fset-value-text-node
           #:make-fset-uri-text-node
           #:make-fset-value-text-nodes

           #:crlf?
           #:qsafe-char?
           #:safe-char?
           #:param-values?
           #:param?
           #:group?

           #:split-string

           #:name?

           #:value-text-node?
           #:uri-text-node?

           #:geo?
           #:uid?
           #:url?
           #:version?
           #:prodid?
           
           #:x-name-line?
           #:content-line?))

(cl:in-package :soiree-parse)

(defparameter *default-namespace* nil)

;;; STP helper functions
(defun make-text-node (text &optional (name "text"))
  (stp:append-child
   (stp:make-element "text" *default-namespace*)
   (stp:make-text text)))

;;; functions for wrapping stp elements inside fset sets
(defun wrap-stp-element (stp-element)
  (fset:map (:node stp-element) (:children (fset:seq))))

(defun unwrap-stp-element (fset-element)
  (fset:reduce (lambda (node x)
                 (stp:append-child
                  node
                  (typecase x
                    (fset:map (unwrap-stp-element x))
                    (t x))))
               (fset:@ fset-element :children)
               :initial-value (fset:@ fset-element :node)))

(defun make-fset-element (name &optional (uri "" uri-supplied-p))
  (wrap-stp-element (apply #'stp:make-element name
                           (when uri-supplied-p (list uri)))))

(defun add-fset-element-child (element child)
  (fset:appendf (fset:@ element :children) (list child))
  element)

(defun make-fset-text (string)
  (wrap-stp-element (stp:make-text string)))

(defun make-fset-text-node (element-tag string)
  (add-fset-element-child
   (make-fset-element element-tag *default-namespace*)
   (make-fset-text string)))

(defun make-fset-text-nodes (element-tag &rest strings)
  (reduce (lambda (element x)
            (add-fset-element-child
             element
             (make-fset-text x)))
          strings
          :initial-value (make-fset-element element-tag *default-namespace*)))

(defun make-fset-value-text-node (element-tag string)
  (add-fset-element-child
   (make-fset-element element-tag *default-namespace*)
   (add-fset-element-child
    (make-fset-element "text" *default-namespace*)
    (make-fset-text string))))

(defun make-fset-uri-text-node (element-tag string)
  (add-fset-element-child
   (make-fset-element element-tag *default-namespace*)
   (add-fset-element-child
    (make-fset-element "uri" *default-namespace*)
    (make-fset-text string))))

(defun make-fset-value-text-nodes (element-tag &rest strings)
  (reduce (lambda (element x)
            (add-fset-element-child
             element
             (add-fset-element-child
              (make-fset-element "text" *default-namespace*)
              (make-fset-text x))))
          strings
          :initial-value (make-fset-element element-tag *default-namespace*)))

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
    (between? (qsafe-char?) 1 nil 'string)
    #\")))

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

(defun value-text-node? (field-name element-tag)
  (named-seq?
   (<- result (content-line? field-name))
   (destructuring-bind (group name params value)
       result
     (apply #'make-fset-value-text-nodes element-tag (split-string value)))))

(defun uri-text-node? (field-name element-tag)
  (named-seq?
   (<- result (content-line? field-name))
   (destructuring-bind (group name params value)
       result
     (make-fset-uri-text-node element-tag value))))

(defun geo? () (uri-text-node? "GEO" "geo"))

(defun uid? () (uri-text-node? "UID" "uid"))
(defun url? () (uri-text-node? "URL" "url"))

(defun version? () (chook? nil (content-line? "VERSION")))
(defun prodid? () (value-text-node? "PRODID" "prodid"))

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

(defun content-line? (&optional name)
  ;; [group "."] name *(";" param) ":" value CRLF
  (named-seq?
   (<- group (opt? (hook?
                    #'first
                    (seq-list? (group?) "."))))
   (<- name (if name name (name?)))
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

