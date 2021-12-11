(in-package :utils)

(defun read-input (name)
  (with-open-file (stream (format nil "input/~a" name))
    (let ((str (make-string (file-length stream))))
      (read-sequence str stream)
      (subseq str 0 (1- (length str))))))

(defun read-lines (name)
  (str:split #\Newline (read-input name)))

(defun read-numbers (name)
  (mapcar #'parse-integer (cl-ppcre:split "[^0-9]+" (read-input name))))

(defun read-grid (input type)
  (let ((lines (str:split #\Newline input)))
    (make-array (list (length lines)
                      (length (first lines)))
                :initial-contents (cond ((eq type :integer) (mapcar (lambda (row)
                                                                      (map 'list (lambda (ch) (- (char-code ch) (char-code #\0))) row))
                                                                    lines))))))
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

(defmacro if-let ((var test-form) then-form &optional else-form)
  `(let ((,var ,test-form))
     (if ,var ,then-form ,else-form)))
