
(in-package :cl-vcard)

(defun  qsafe-char-p (char)
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

(defun split-string (str &optional (escape-char #\\))
  (let (escaped acc cur)
    (loop for c across str
       do (if escaped
              (progn
                (push c cur)
                (setf escaped nil))
              (progn
                (cond ((eql c #\,)
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

(defun name? () (word?))

(defun content-line? ()
  ;; [group "."] name *(";" param) ":" value CRLF
  (named-seq?
   (<- group (opt? (seq-list? (group?) ".")))
   (<- name (name?))
   (<- params (many? (named-seq?
                      ";"
                      (<- param (param?))
                      param)))
   ":"
   (<- value (value?))
   #\Return #\Newline
   (list name value params group)))

(defun vcard? ()
  (named-seq?
   "BEGIN" ":" "VCARD" #\Return #\Newline
   (<- content (many1? (content-line?)))
   "END" ":" "VCARD" #\Return #\Newline
   content))

(defun parse-vcard (str)
  (parse-string* (vcard?) str))
