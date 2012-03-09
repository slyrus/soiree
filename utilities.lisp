
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


