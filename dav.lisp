
(cl:defpackage #:soiree-dav
  (:use #:cl #:soiree)
  (:export #:make-drakma-dav-server-connection
           #:dav-host
           #:dav-port
           #:dav-user
           #:dav-password
           #:dav-url
           #:dav-server-connection-url

           #:*dav-xml-namespace*
           #:*carddav-xml-namespace*
           #:*caldav-xml-namespace*
           
           #:with-carddav-namespaces
           #:with-caldav-namespaces

           #:get-addressbook-home-set
           #:get-addressbook-collections
           #:get-addressbook-collection
           #:get-addressbook-collection-filter

           #:get-calendar-home-set
           #:get-calendar-collections
           #:get-calendar-collection))

(cl:in-package #:soiree-dav)

(defparameter *dav-xml-namespace* "DAV:")

(defclass dav-server-connection () 
  ((host :initarg :host :accessor dav-host)
   (port :initarg :port :accessor dav-port)
   (user :initarg :user :accessor dav-user)
   (password :initarg :password :accessor dav-password)
   (url :initarg :url :accessor dav-url)))

(defgeneric dav-request (connection url method content &key depth))

(defclass drakma-dav-server-connection (dav-server-connection)
  ((use-ssl :accessor dav-use-ssl-p :initarg :use-ssl :initform t)))

(defun make-drakma-dav-server-connection (&key host port user password url)
  (make-instance 'drakma-dav-server-connection
                 :host host
                 :port port
                 :user user
                 :password password
                 :url url))

(defun dav-server-connection-url (connection &optional (url (dav-url connection)))
  (concatenate 'string "https://"
               (dav-host connection) ":"
               (format nil "~A" (dav-port connection))
               url))

(defmethod dav-request ((connection drakma-dav-server-connection) url method content
                        &key depth (connection-timeout 120))
  (multiple-value-bind (reply status server-headers)
      (apply #'drakma:http-request
             (apply #'dav-server-connection-url connection
                    (when url `(,url)))
             :force-ssl (dav-use-ssl-p connection)
             :method method
             :basic-authorization `(,(dav-user connection) ,(dav-password connection))
             :content content
             :connection-timeout connection-timeout
             (when depth
               `(:additional-headers (("Depth" . ,depth)))))
    (declare (ignore status server-headers))
    reply))

(defun write-stp-string (node)
  (let* ((stream (make-string-output-stream))
         (sink (cxml:make-character-stream-sink stream :canonical nil :indentation 2)))
    (stp:serialize node sink)
    (get-output-stream-string stream)))

(defun write-xmls-string (node &key (canonical t) (indentation nil))
  (let* ((stream (make-string-output-stream))
         (sink (cxml:make-character-stream-sink stream :canonical canonical :indentation indentation)))
    (cxml-xmls:map-node sink node)
    (get-output-stream-string stream)))

