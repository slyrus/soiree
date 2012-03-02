
(in-package :cl-vcard)

(defparameter *vcard-namespace* "urn:ietf:params:xml:ns:vcard-4.0")

(defmacro when-string (test &body forms)
  (let ((str (gensym)))
    `(let ((,str ,test))
       (when (and ,str (not (equal ,str "")))
         ,@forms))))

(defun wrap-stp-element (stp-element)
  (fset:map (:node stp-element) (:children (fset:seq))))

(defun unwrap-stp-element (fset-element)
  (fset:reduce (lambda (node x)
                 (stp:append-child
                  node
                  (unwrap-stp-element x)))
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

(defun make-text-node (element-tag string)
  (add-fset-element-child
   (make-fset-element element-tag *vcard-namespace*)
   (make-fset-text string)))

(defun make-text-nodes (element-tag &rest strings)
  (reduce (lambda (element x)
            (add-fset-element-child
             element
             (make-fset-text x)))
          strings
          :initial-value (make-fset-element element-tag *vcard-namespace*)))

(defun make-value-text-node (element-tag string)
  (add-fset-element-child
   (make-fset-element element-tag  *vcard-namespace*)
   (add-fset-element-child
    (make-fset-element "text" *vcard-namespace*)
    (make-fset-text string))))

(defun make-uri-text-node (element-tag string)
  (add-fset-element-child
   (make-fset-element element-tag  *vcard-namespace*)
   (add-fset-element-child
    (make-fset-element "uri" *vcard-namespace*)
    (make-fset-text string))))

(defun make-value-text-nodes (element-tag &rest strings)
  (reduce (lambda (element x)
            (add-fset-element-child
             element
             (add-fset-element-child
              (make-fset-element "text" *vcard-namespace*)
              (make-fset-text x))))
          strings
          :initial-value (make-fset-element element-tag *vcard-namespace*)))

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

(defun param-values? ()
  (hook? #'split-string
         (param-value?)))

(defun param? ()
  (named-seq?
   (<- param-name (param-name?))
   #\=
   (<- param-values (param-values?))
   (list param-name param-values)))

(defun non-ascii-p (char)
  (<= #x80 (char-code char) #xff))

(def-cached-parser non-ascii?
  (sat #'non-ascii-p))

(defun wsp-p (char)
  (or (eql char #\space)
      (eql char #\tab)))

(def-cached-parser wsp?
  (sat #'wsp-p))

(def-cached-parser value?
  (between? (choices (vchar?) (wsp?) (non-ascii?))
            1 nil 'string))

(defun name? ()
  (between? (alphanum-or-dash?) 1 nil 'string))

(defun x-name? ()
  (named-seq?
   (<- x (choice #\X #\x))
   #\-
   (<- name (between? (alphanum-or-dash?) 1 nil 'string))
   (concatenate 'string (string x) "-" name)))

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

(defun version? ()
  (named-seq?
   (content-line? "VERSION")
   nil))

(defun value-text-node? (vcard-field-name element-tag)
  (named-seq?
   (<- result (content-line? vcard-field-name))
   (destructuring-bind (group name params value)
       result
     (apply #'make-value-text-nodes element-tag (split-string value)))))

(defun uri-text-node? (vcard-field-name element-tag)
  (named-seq?
   (<- result (content-line? vcard-field-name))
   (destructuring-bind (group name params value)
       result
     (make-uri-text-node element-tag value))))

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
(defun geo? () (uri-text-node? "GEO" "geo"))
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

(defun prodid? () (value-text-node? "PRODID" "prodid"))
(defun related? () (value-text-node? "RELATED" "related"))
(defun rev? () (value-text-node? "REV" "rev"))

(defun role? () (value-text-node? "ROLE" "role"))
(defun gender? () (value-text-node? "GENDER" "gender"))
(defun sound? () (value-text-node? "SOUND" "sound"))

(defun source? () (value-text-node? "SOURCE" "source"))
(defun tel? () (value-text-node? "TEL" "tel"))
(defun title? () (value-text-node? "TITLE" "title"))

(defun tz? () (value-text-node? "TZ" "tz"))
(defun uid? () (uri-text-node? "UID" "uid"))
(defun url? () (uri-text-node? "URL" "url"))

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
  (stp:make-document
   (fset:reduce (lambda (element x)
                  (stp:append-child
                   element
                   (unwrap-stp-element x)))
                (parse-string* (many1? (vcard?)) str)
                :initial-value (stp:make-element "vcards" *vcard-namespace*))))

