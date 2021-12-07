(in-package :day7)

(defvar *test-input* (utils:read-numbers "day7_test.txt"))
(defvar *input* (utils:read-numbers "day7.txt"))

(defun linear-dist (x y)
  (abs (- x y)))

(defun triangular-dist (x y)
  (let ((d (abs (- x y))))
    (* d (1+ d) 1/2)))

(defun calculate-required-fuel (middle crabs distfn)
  (loop for crab in crabs
        sum (funcall distfn middle crab)))

(defun part1 (input)
  (let* ((crabs (sort (apply #'list input) #'<))
         (middle (nth (round (/ (length crabs) 2)) crabs)))
    (calculate-required-fuel middle crabs #'linear-dist)))

(defun part2 (input)
  (let ((middle (/ (apply #'+ input) (length input))))
    (apply #'min (mapcar (lambda (mid)
                           (calculate-required-fuel mid input #'triangular-dist))
                         (list (floor middle) (ceiling middle))))))

(define-test day7
  (is = 37 (part1 *test-input*))
  (is = 168 (part2 *test-input*)))
