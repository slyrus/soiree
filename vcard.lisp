
(in-package :cl-vcard)

(defun group? () (word?))
(defun param-name? () (word?))
(defun param-value? () (word?))

(defun param? ()
  (named-seq?
   (<- param-name (param-name?))
   #\=
   (<- param-value (param-value?))
   (list param-name param-value)))

(defun vcharp (char)
  (<= #x21 (char-code char) #x7e))

(def-cached-parser vchar?
  (sat #'vcharp))

(defun non-ascii-p (char)
  (<= #x21 (char-code char) #x7e))

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

(defun <content-line> ()
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
   (<- content (many? (<content-line>)))
   "END" ":" "VCARD" #\Return #\Newline
   content))

(defun parse-vcard (str)
  (parse-string* (vcard?) str))
