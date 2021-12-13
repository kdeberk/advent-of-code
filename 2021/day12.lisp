(in-package :day12)

(defvar *test-input-1* (utils:read-lines "day12_test1.txt"))
(defvar *test-input-2* (utils:read-lines "day12_test2.txt"))
(defvar *test-input-3* (utils:read-lines "day12_test3.txt"))
(defvar *input* (utils:read-lines "day12.txt"))

(defstruct (cave (:conc-name nil)
                 (:print-function (lambda (cave out depth)
                                    (declare (ignore depth)) ;; Needed to prevent an infinite loop when printing.
                                    (format out "#S(cave :NAME ~a :NEIGHBORS ~a)" (name cave) (mapcar #'name (neighbors cave))))))
  cave-name small? start? end? neighbors visits)

(defmacro while-visiting-cave (cave &body body)
  `(progn (incf (visits ,cave))
          (let ((result (progn ,@body)))
            (decf (visits ,cave))
            result)))

(defun read-caves (lines)
  (let ((pairs (mapcar (lambda (line) (str:split "-" line)) lines)))
    (let ((caves (mapcar (lambda (name)
                           (make-cave :cave-name name
                                      :small? (string= (string-downcase name) name)
                                      :start? (string= "start" name)
                                      :end? (string= "end" name)
                                      :neighbors nil
                                      :visits 0))
                         (remove-duplicates (append (mapcar #'car pairs) (mapcar #'cadr pairs))
                                            :test #'string=))))
      (loop for (a b) in pairs
            do (let ((cave-a (find a caves :key #'cave-name :test #'string=))
                     (cave-b (find b caves :key #'cave-name :test #'string=)))
                 (push cave-a (neighbors cave-b))
                 (push cave-b (neighbors cave-a))))
      caves)))

(defun part1 (input)
  (let ((caves (read-caves input)))
    (let ((start (find t caves :key #'start?)))
      (labels ((count-paths (cur)
                 (cond ((end? cur) 1)
                       ((and (small? cur) (< 0 (visits cur))) 0)
                       (t (while-visiting-cave cur
                            (loop for next in (neighbors cur)
                                  unless (start? next)
                                    sum (count-paths next)))))))
        (count-paths start)))))

(defun part2 (input)
  (let ((caves (read-caves input)))
    (let ((start (find t caves :key #'start?)))
      (labels ((count-paths (cur &optional double-visited-earlier)
                 (cond ((end? cur) 1)
                       ((and (small? cur) (< 0 (visits cur)) double-visited-earlier) 0)
                       (t (while-visiting-cave cur
                            (loop for next in (neighbors cur)
                                  unless (start? next)
                                    sum (count-paths next (or double-visited-earlier
                                                              (and (small? cur) (< 1 (visits cur)))))))))))
        (count-paths start)))))

(define-test day12
  (is = 10 (part1 *test-input-1*))
  (is = 36 (part2 *test-input-1*))
  (is = 19 (part1 *test-input-2*))
  (is = 103 (part2 *test-input-2*))
  (is = 226 (part1 *test-input-3*))
  (is = 3509 (part2 *test-input-3*)))
