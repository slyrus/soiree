
(in-package :soiree-dav)

(defparameter *carddav-xml-namespace* "urn:ietf:params:xml:ns:carddav")

(defmacro with-carddav-namespaces (&body body)
  `(xpath:with-namespaces (("C" *carddav-xml-namespace*)
                           (nil *dav-xml-namespace*))
    
     ,@body))

(defun get-addressbook-home-set (connection)
  (let ((result
          (dav-request connection nil :propfind
                       (write-xmls-string
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
          (dav-request connection home-set :propfind
                       (write-xmls-string
                        `(("propfind" . ,*dav-xml-namespace*) nil
                          (("allprop" . ,*dav-xml-namespace*) nil)))
                                           :depth 1)))
    (when response
      (let ((parsed (cxml:parse response (stp:make-builder))))
        (xpath:with-namespaces ((nil *dav-xml-namespace*)
                                ("CAL" "urn:ietf:params:xml:ns:caldav")
                                ("CARD" *carddav-xml-namespace*))
          (xpath:map-node-set->list
           #'xpath:string-value
           (xpath:evaluate 
            "/multistatus/response[propstat/prop/resourcetype/CARD:addressbook]/href"
            parsed)))))))

(defun get-addressbook-collection (connection collection)
  (let ((response
          (dav-request connection collection :report
                       (write-xmls-string
                        `(("addressbook-query" . ,*carddav-xml-namespace*) nil
                          (("prop" . ,*dav-xml-namespace*) nil
                           (("getetag" . ,*dav-xml-namespace*) nil)
                           (("address-data" . ,*carddav-xml-namespace*) nil)))))))
    (when response
      (cxml:parse response (stp:make-builder)))))

(defun get-addressbook-collection-filter (connection
                                          collection
                                          match-text
                                          &key (match-type :contains)
                                               (filter-field "FN"))
  (let ((response
          (dav-request connection collection :report
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

