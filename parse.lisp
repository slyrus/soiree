
(in-package :cl-vcard)

(defparameter *vcard-namespace* "urn:ietf:params:xml:ns:vcard-4.0")

(defmacro when-string (test &body forms)
  (let ((str (gensym)))
    `(let ((,str ,test))
       (when (and ,str (not (equal ,str "")))
         ,@forms))))

(defun make-text-node (element-tag string)
  (stp:append-child
   (stp:make-element element-tag  *vcard-namespace*)
   (stp:append-child
    (stp:make-element "text" *vcard-namespace*)
    (stp:make-text string))))

(defun make-text-nodes (element-tag &rest strings)
  (reduce (lambda (element x)
            (stp:append-child
             element
             (stp:append-child
              (stp:make-element "text" *vcard-namespace*)
              (stp:make-text x))))
          strings
          :initial-value (stp:make-element element-tag *vcard-namespace*)))

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

(defun group? () (word?))
(defun param-name? () (word?))

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
                   (push c cur)
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

(defun long-line-extension? ()
  (named-seq?
   #\Return
   #\Newline
   (wsp?)
   (<- value (value?))
   value))

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

(defun text-node? (vcard-field-name element-tag)
  (named-seq?
   (<- result (content-line? vcard-field-name))
   (destructuring-bind (group name params value)
       result
     (make-text-node element-tag value))))

(defun adr? ()
  (named-seq?
   (<- result (content-line? "ADR"))
   (destructuring-bind (group name params value)
       result
     (destructuring-bind (pobox ext street locality region code country)
         (split-string value :delimiter #\;)
       (let ((adr-element (stp:make-element "property-adr" *vcard-namespace*)))
         (when-string pobox
           (stp:append-child adr-element
                             (apply #'make-text-nodes "pobox"
                                    (split-string pobox))))
         (when-string ext
           (stp:append-child adr-element
                             (apply #'make-text-nodes "ext"
                                    (split-string ext))))
         (when-string street
           (stp:append-child adr-element
                             (apply #'make-text-nodes "street"
                                    (split-string street))))
         (when-string locality
           (stp:append-child adr-element
                             (apply #'make-text-nodes "locality"
                                    (split-string locality))))
         (when-string region
           (stp:append-child adr-element
                             (apply #'make-text-nodes "region"
                                    (split-string region))))
         (when-string code
           (stp:append-child adr-element
                             (apply #'make-text-nodes "code"
                                    (split-string code))))
         (when-string country
           (stp:append-child adr-element
                             (apply #'make-text-nodes "country"
                                    (split-string country))))

         adr-element)))))

(defun anniversary? () (text-node? "ANNIVERSARY" "property-anniversary"))
(defun bday? () (text-node? "BDAY" "property-bday"))
(defun caladruri? () (text-node? "CALADRURI" "property-caladruri"))
(defun caluri? () (text-node? "CALURI" "property-caluri"))

;; fix me -- categories needs to accept multiple values
(defun categories? () (text-node? "CALURI" "property-categories"))

(defun clientpidmap? () (text-node? "CLIENTPIDMAP" "property-clientpidmap"))
(defun email? () (text-node? "EMAIL" "property-email"))
(defun fburl? () (text-node? "FBURL" "property-fburl"))

(defun fn? () (text-node? "FN" "property-fn"))
(defun geo? () (text-node? "GEO" "property-geo"))
(defun impp? () (text-node? "IMPP" "property-impp"))
(defun key? () (text-node? "KEY" "property-key"))

(defun kind? () (text-node? "KIND" "property-kind"))
(defun lang? () (text-node? "LANG" "property-lang"))
(defun logo? () (text-node? "LOGO" "property-logo"))

(defun member? () (text-node? "MEMBER" "property-member"))
(defun nickname? () (text-node? "NICKNAME" "property-nickname"))

(defun note? () (text-node? "NOTE" "property-note"))
(defun org? () (text-node? "ORG" "property-org"))
(defun photo? () (text-node? "PHOTO" "property-photo"))

(defun prodid? () (text-node? "PRODID" "property-prodid"))
(defun related? () (text-node? "RELATED" "property-related"))
(defun rev? () (text-node? "REV" "property-rev"))

(defun role? () (text-node? "ROLE" "property-role"))
(defun gender? () (text-node? "GENDER" "property-gender"))
(defun sound? () (text-node? "SOUND" "property-sound"))

(defun source? () (text-node? "SOURCE" "property-source"))
(defun tel? () (text-node? "TEL" "property-tel"))
(defun title? () (text-node? "TITLE" "property-title"))

(defun tz? () (text-node? "TZ" "property-tz"))
(defun uid? () (text-node? "UID" "property-uid"))
(defun url? () (text-node? "URL" "property-url"))

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
       (let ((n-element (stp:make-element "property-n" *vcard-namespace*)))
         (when-string family-names
           (stp:append-child n-element
                             (apply #'make-text-nodes "surname"
                                    (split-string family-names))))
         (when-string given-names
           (stp:append-child n-element
                             (apply #'make-text-nodes "given"
                                    (split-string given-names))))
         (when-string additional-names
           (stp:append-child n-element
                             (apply #'make-text-nodes "additional"
                                    (split-string additional-names))))
         (when-string honorific-prefixes
           (stp:append-child n-element
                             (apply #'make-text-nodes "prefixes"
                                    (split-string honorific-prefixes))))
         (when-string honorific-suffixes
           (stp:append-child n-element
                             (apply #'make-text-nodes "suffixes"
                                    (split-string honorific-suffixes))))
         n-element)))))

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

                 (clientpidmap?)
                 (email?)
                 (fburl?)

                 (fn?)
                 (geo?)
                 (impp?)
                 (key?)

                 (kind?)
                 (lang?)
                 (logo?)

                 (member?)
                 (n?)
                 (nickname?)

                 (note?)
                 (org?)
                 (photo?)

                 (prodid?)
                 (related?)
                 (rev?)

                 (role?)
                 (gender?)
                 (sound?)

                 (source?)
                 (tel?)
                 (title?)

                 (tz?)
                 (uid?)
                 (url?)

                 (content-line?))))

   "END" ":" "VCARD" #\Return #\Newline
   (reduce (lambda (element x)
             (if (typep x 'stp:element)
                 (stp:append-child element x)
                 element))
           content
           :initial-value (stp:make-element "vcard" *vcard-namespace*))))

(defun parse-vcard (str)
  (stp:make-document
   (reduce (lambda (element x)
             (stp:append-child
              element
              x))
           (parse-string* (many1? (vcard?)) str)
           :initial-value (stp:make-element "vcards" *vcard-namespace*))))

