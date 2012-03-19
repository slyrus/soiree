
(in-package :soiree)

(defun contents-of-stream (in)
  "Returns a string with the entire contents of the specified file."
  (with-output-to-string (contents)
    (let* ((buffer-size 4096)
           (buffer (make-string buffer-size)))
      (labels ((read-chunks ()
                 (let ((size (read-sequence buffer in)))
                   (if (< size buffer-size)
                       (princ (subseq buffer 0 size) contents)
                       (progn
                         (princ buffer contents)
                         (read-chunks))))))
        (read-chunks)))))

(defun contents-of-file (pathname)
  (with-open-file (in pathname :direction :input)
    (contents-of-stream in)))

(defmacro when-string (test &body forms)
  (let ((str (gensym)))
    `(let ((,str ,test))
       (when (and ,str (not (equal ,str "")))
         ,@forms))))

(defun remove-keyword (key args)
  (let ((pos (position key args)))
    (if (and pos (evenp pos))
        (concatenate (type-of args)
                     (subseq args 0 pos)
                     (subseq args (+ pos 2)))
        args)))

(defun keep (item sequence &rest args &key from-end (test 'eql) test-not start
                                           end count key)
  (declare (ignorable from-end test-not start end count key))
  (apply #'remove-if-not (lambda (x) (funcall test x item))
         sequence (remove-keyword :test args)))

(defmacro when-let ((var form) &body body)
  `(let ((,var ,form))
     (when ,var
       ,@body)))

(defun convert-string-to-dos-line-endings (string)
  (with-output-to-string (out)
    (with-input-from-string (stream string)
      (loop for c = (read-char stream nil nil)
            while c
            do (cond ((eq c #\return)
                      (write-char #\return out)
                      (let ((next (read-char stream)))
                        (write-char #\linefeed out)
                        (unless (eq next #\linefeed)
                          (write-char next out))))
                     ((eq c #\linefeed)
                      (write-char #\return out)
                      (write-char #\linefeed out))
                     (t (write-char c out)))))))
