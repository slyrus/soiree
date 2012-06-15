
(cl:defpackage #:soiree-dav
  (:use #:cl #:soiree)
  (:export #:make-dav-server-connection
           #:dav-host
           #:dav-port
           #:dav-user
           #:dav-password
           #:dav-url

           #:*dav-xml-namespace*
           #:*carddav-xml-namespace*

           #:get-addressbook-collection
           #:get-addressbook-collections
           #:get-addressbook-home-set))

(cl:in-package #:soiree-dav)

(defparameter *dav-xml-namespace* "DAV:")

(defstruct (dav-server-connection (:conc-name dav-))
  host
  port
  user
  password
  url)

(defun write-stp-string (node)
  (let* ((stream (make-string-output-stream))
         (sink (cxml:make-character-stream-sink stream :canonical nil :indentation 2)))
    (stp:serialize node sink)
    (get-output-stream-string stream)))

(defun write-xmls-string (node)
  (let* ((stream (make-string-output-stream))
         (sink (cxml:make-character-stream-sink stream :canonical nil :indentation 2)))
    (cxml-xmls:map-node sink node)
    (get-output-stream-string stream)))

