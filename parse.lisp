
(in-package :soiree)

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

