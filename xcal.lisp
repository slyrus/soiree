(cl:defpackage #:soiree-xcal
  (:use #:common-lisp #:parser-combinators #:soiree-parse)
  (:export #:xcal-date-to-universal-time
           #:xcal-date-time-to-universal-time))

(cl:in-package :soiree-xcal)

;; xcal support

(defun xcal-date? ()
  (hook? (lambda (x)
           (destructuring-bind (year-digits month-digits day-digits) x
             (let ((year (digits-to-number year-digits))
                   (month (+ (* 10 (first month-digits)) (second month-digits)))
                   (day (+ (* 10 (first day-digits)) (second day-digits))))
               (list year month day))))
         (named-seq? (<- year (times? (hook? #'digit-char-p (digit?)) 4))
                     #\-
                     (<- month (times? (hook? #'digit-char-p (digit?)) 2))
                     #\-
                     (<- day (times? (hook? #'digit-char-p (digit?)) 2))
                     (list year month day))))

(defun xcal-time? ()
  (hook? (lambda (x)
           (destructuring-bind (hour-digits minute-digits second-digits time-utc) x
             (let ((hour (digits-to-number hour-digits))
                   (minute (+ (* 10 (first minute-digits)) (second minute-digits)))
                   (second (+ (* 10 (first second-digits)) (second second-digits))))
               (list hour minute second time-utc))))
         (named-seq? (<- hour (times? (hook? #'digit-char-p (digit?)) 2))
                     #\:
                     (<- minute (times? (hook? #'digit-char-p (digit?)) 2))
                     #\:
                     (<- sec (times? (hook? #'digit-char-p (digit?)) 2))
                     (<- utc (opt? #\Z))
                     (list hour minute sec utc))))

(defun xcal-date-time? ()
  (named-seq?
   (<- date (xcal-date?))
   #\T
   (<- time (xcal-time?))
   (list date time)))

(defun xcal-date-to-universal-time (xcal-date &key (time-zone nil))
  (destructuring-bind (year month date)
      (parse-string* (xcal-date?) xcal-date)
    (apply #'encode-universal-time 0 0 0 date month year (list time-zone))))

(defun xcal-date-time-to-universal-time (xcal-date-time &key (time-zone nil))
  (destructuring-bind ((year month date) (hour minute second utc))
      (parse-string* (xcal-date-time?) xcal-date-time)
    (apply #'encode-universal-time second minute hour date month year (list (if utc 0 time-zone)))))

