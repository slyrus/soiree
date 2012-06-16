
(in-package :soiree-dav)

(defparameter *caldav-xml-namespace* "urn:ietf:params:xml:ns:caldav")

(defmacro with-caldav-namespaces (&body body)
  `(xpath:with-namespaces (("C" *caldav-xml-namespace*)
                           (nil *dav-xml-namespace*))
     
     ,@body))

(defun get-calendar-home-set (connection)
  (let ((result
          (dav-request connection nil :propfind
                       (write-xmls-string
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
          (dav-request connection home-set :propfind
                       (write-xmls-string
                        `(("propfind" . ,*dav-xml-namespace*) nil
                          (("allprop" . ,*dav-xml-namespace*) nil)))
                                           :depth 1)))
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
  (let ((response
          (dav-request connection collection :report
                       (write-xmls-string
                        `(("calendar-query" . ,*caldav-xml-namespace*) nil
                          (("prop" . ,*dav-xml-namespace*) nil
                           (("getetag" . ,*dav-xml-namespace*) nil)
                           (("calendar-data" . ,*caldav-xml-namespace*) nil)))))))
    (when response
      (cxml:parse response (stp:make-builder)))))
