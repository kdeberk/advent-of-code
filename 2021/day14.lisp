(in-package :day14)

(defvar *test-input* (utils:read-input "day14_test.txt"))
(defvar *input* (utils:read-input "day14.txt"))

(defun read-puzzle (input)
  (destructuring-bind (start _ &rest steps) (str:split #\Newline input)
    (declare (ignore _))
    (list (coerce start 'list)
          (mapcar (lambda (step)
                    (destructuring-bind (pair insert) (str:split " -> " step)
                      (cons (coerce pair 'list) (char insert 0))))
                  steps))))

(defun perform-steps (start steps count)
  (dotimes (_ count)
    (let ((cur start))
      (loop
        (let ((next (cdr cur)))
          (unless next
            (return))
          (let ((insert (cdr (assoc (list (car cur) (car next)) steps :test #'equal))))
            (setf (cdr cur) (cons insert next)
                  cur next))))))
  start)

(defun count-letters (sequence)
  (let ((counts))
    (dolist (cur sequence)
      (utils:push-or-inc cur counts))
    counts))

(defun diff-most-and-least-common (counts)
  (- (apply #'max (mapcar #'cdr counts))
     (apply #'min (mapcar #'cdr counts))))

(defun part1 (input)
  (destructuring-bind (sequence steps) (read-puzzle input)
    (let ((counts (count-letters (perform-steps sequence steps 10))))
      (diff-most-and-least-common counts))))

(defun part2 (input)
  (destructuring-bind (sequence steps) (read-puzzle input)
    (let ((count-lookup-table))
      (labels ((descend-pair (pair depth)
                 (destructuring-bind (a b) pair
                   (or (cdr (assoc (list pair depth) count-lookup-table :test #'equal))
                       (let ((expanded (perform-steps (list a b) steps 1)))
                         (let ((counts (if (= 40 depth)
                                           (count-letters expanded)
                                           (descend-sequence expanded depth))))
                           (push (cons (list pair depth) counts) count-lookup-table)
                           counts)))))
               (descend-sequence (sequence depth)
                 (let ((summed))
                   (loop for (a b c) on sequence
                         when b
                           do (loop for (letter . count) in (descend-pair (list a b) (1+ depth))
                                    do (utils:push-or-inc letter summed :by count)
                                    finally (when c (decf (cdr (assoc b summed))))))  ;; For most of the second letters, we are counting them too much since they are also included in the next pair
                   summed)))
        (diff-most-and-least-common (descend-sequence sequence 0))))))

(define-test day14
  (is = 1588 (part1 *test-input*))
  (is = 2188189693529 (part2 *test-input*)))
