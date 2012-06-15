
(in-package :soiree-dav)

(defparameter *caldav-xml-namespace* "urn:ietf:params:xml:ns:caldav")

(defmacro with-caldav-namespaces (&body body)
  `(xpath:with-namespaces (("C" *caldav-xml-namespace*)
                           (nil *dav-xml-namespace*))
     
     ,@body))

(defun get-calendar-home-set (connection)
  (let ((result
          (drakma:http-request
           (concatenate 'string 
                        "https://"
                        (dav-host connection)
                        ":"
                        (format nil "~A" (dav-port connection))
                        (dav-url connection))
           :force-ssl t
           :method :propfind
           :basic-authorization `(,(dav-user connection) ,(dav-password connection))
           :content (write-xmls-string
                     `(("propfind" . ,*dav-xml-namespace*) nil
                       (("prop" . ,*dav-xml-namespace*) nil
                        (("calendar-home-set" . ,*caldav-xml-namespace*) nil)))))))
    (when result
      (let ((parsed (cxml:parse result (stp:make-builder))))
        (xpath:with-namespaces ((nil *dav-xml-namespace*)
                                ("C" *caldav-xml-namespace*))
          (xpath:string-value
           (xpath:evaluate "/multistatus/response/propstat/prop/C:calendar-home-set/href"
                           parsed)))))))

(defun get-calendar-collections (connection home-set)
  (let ((response
          (drakma:http-request
           (concatenate 'string 
                        "https://"
                        (dav-host connection)
                        ":"
                        (format nil "~A" (dav-port connection))
                        home-set)
           :force-ssl t
           :method :propfind
           :basic-authorization `(,(dav-user connection) ,(dav-password connection))
           :additional-headers '(("Depth" . 1))
           :content
            (write-xmls-string
             `(("propfind" . ,*dav-xml-namespace*) nil
               (("allprop" . ,*dav-xml-namespace*) nil))))))
    (when response
      (let ((parsed (cxml:parse response (stp:make-builder))))
        (xpath:with-namespaces ((nil *dav-xml-namespace*)
                                ("C" "urn:ietf:params:xml:ns:caldav"))
          (xpath:map-node-set->list
           #'xpath:string-value
           (xpath:evaluate 
            "/multistatus/response[propstat/prop/resourcetype/C:calendar]/href"
            parsed)))))))

(defun get-calendar-collection (connection collection)
  (let ((response (drakma:http-request
                   (concatenate 'string 
                                "https://"
                                (dav-host connection)
                                ":"
                                (format nil "~A" (dav-port connection))
                                collection)
                   :force-ssl t
                   :method :report
                   :basic-authorization `(,(dav-user connection) ,(dav-password connection))
                   :content
                   (write-xmls-string
                    `(("calendar-query" . ,*caldav-xml-namespace*) nil
                      (("calendar-data" . ,*caldav-xml-namespace*) nil
                       (("getetag" . ,*dav-xml-namespace*) nil
                        (("prop" . ,*dav-xml-namespace*) nil))))))))
    (when response
      (cxml:parse response (stp:make-builder)))))
