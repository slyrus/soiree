
(in-package :soiree-dav)

(defparameter *carddav-xml-namespace* "urn:ietf:params:xml:ns:carddav")

(defmacro with-carddav-namespaces (&body body)
  `(xpath:with-namespaces (("C" *carddav-xml-namespace*)
                           (nil *dav-xml-namespace*))
    
     ,@body))

(defun get-addressbook-home-set (connection)
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
                        (("addressbook-home-set" . ,*carddav-xml-namespace*) nil)))))))
    (when result
      (let ((parsed (cxml:parse result (stp:make-builder))))
        (xpath:with-namespaces ((nil *dav-xml-namespace*)
                                ("VC" *carddav-xml-namespace*))
          (xpath:string-value
           (xpath:evaluate "/multistatus/response/propstat/prop/VC:addressbook-home-set/href"
                           parsed)))))))

(defun get-addressbook-collections (connection home-set)
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
                                ("C" "urn:ietf:params:xml:ns:caldav")
                                ("C1" *carddav-xml-namespace*))
          (xpath:map-node-set->list
           #'xpath:string-value
           (xpath:evaluate 
            "/multistatus/response[propstat/prop/resourcetype/C1:addressbook]/href"
            parsed)))))))

(defun get-addressbook-collection (connection collection)
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
                    `(("addressbook-query" . ,*carddav-xml-namespace*) nil
                      (("address-data" . ,*carddav-xml-namespace*) nil
                       (("getetag" . ,*dav-xml-namespace*) nil
                        (("prop" . ,*dav-xml-namespace*) nil))))))))
    (when response
      (cxml:parse response (stp:make-builder)))))

(defun get-addressbook-collection-filter (connection
                                          collection
                                          match-text
                                          &key (match-type :contains)
                                               (filter-field "FN"))
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
                    `(("addressbook-query" . ,*carddav-xml-namespace*) nil
                      (("prop" . ,*dav-xml-namespace*) nil
                       (("getetag" . ,*dav-xml-namespace*) nil)
                       (("address-data" . ,*carddav-xml-namespace*) nil))
                      (("filter" . ,*carddav-xml-namespace*) nil
                       (("prop-filter" . ,*carddav-xml-namespace*)
                        (("name" ,filter-field))
                        (("text-match" . ,*carddav-xml-namespace*)
                         (("collation" "i;unicode-casemap")
                          ("match-type" ,(string-downcase (symbol-name match-type))))
                         ,match-text))))))))
    (when response
      (cxml:parse response (stp:make-builder)))))

