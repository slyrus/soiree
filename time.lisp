
(cl:defpackage #:soiree-time
  (:use #:common-lisp #:soiree)
  (:export #:timestamp<-with-nils
           #:timestamp>-with-nils

           #:iso-8601-basic-time-utc
           #:iso-8601-basic-time-local
           #:asctime))

(cl:in-package :soiree-time)

;; times

(defun timestamp<-with-nils (a b)
  (cond ((null a) t)
        ((null b) nil)
        (t (local-time:timestamp< a b))))

(defun timestamp>-with-nils (a b)
  (cond ((null a) t)
        ((null b) nil)
        (t (local-time:timestamp> a b))))

(defparameter +iso-8601-basic-date-format+
  '((:year 4) (:month 2) (:day 2)))

(defparameter +iso-8601-basic-time-format+
  '((:hour 2) (:min 2) (:sec 2)))

(defparameter +iso-8601-basic-format-utc+
  ;; 20081118T023200Z
  (append +iso-8601-basic-date-format+ (list #\T) +iso-8601-basic-time-format+ (list #\Z)))

(defun iso-8601-basic-time-utc (timestamp &optional stream)
  (when timestamp
    (local-time:format-timestring stream timestamp
                                  :format +iso-8601-basic-format-utc+)))

(defparameter +iso-8601-basic-format-local+
  ;; 20081118T023200
  (append +iso-8601-basic-date-format+ (list #\T) +iso-8601-basic-time-format+))

(defun iso-8601-basic-time-local (timestamp &optional stream)
  (when timestamp
    (local-time:format-timestring stream timestamp
                                  :format +iso-8601-basic-format-local+)))

(defun asctime (timestamp &optional stream)
  (when timestamp
    (local-time:format-timestring stream timestamp
                                  :format local-time:+asctime-format+)))

;; timezones

(defmacro with-ical-namespace (&body body)
  `(xpath:with-namespaces ((nil soiree::*ical-namespace*))
    ,@body))

