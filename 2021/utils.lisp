(in-package :utils)

(defun read-input (name)
  (with-open-file (stream (format nil "input/~a" name))
    (let ((str (make-string (file-length stream))))
      (read-sequence str stream)
      (subseq str 0 (1- (length str))))))

(defun read-lines (name)
  (str:split #\Newline (read-input name)))

(defun read-numbers (name)
  (mapcar #'parse-integer (read-lines name)))

(defmacro stringcase (expr &body entries)
  (let ((val (gensym)))
    `(let ((,val ,expr))
       (cond ,@(mapcar (lambda (entry)
                         (destructuring-bind (string &body body) entry
                           `((string= ,val ,string) ,@body)))
                       entries)))))

(defun partial (fn &rest args1)
  (lambda (&rest args2)
    (apply fn (append args1 args2))))
