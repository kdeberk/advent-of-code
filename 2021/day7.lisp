(in-package :day7)

(defvar *test-input* (utils:read-numbers "day7_test.txt"))
(defvar *input* (utils:read-numbers "day7.txt"))

(defun read-crabs (numbers)
  (let ((pairs))
    (dolist (n numbers)
      (let ((p (assoc n pairs)))
        (if p
            (incf (cdr p))
            (push (cons n 1) pairs))))
    (sort pairs #'< :key #'car)))

(defun linear-dist (x y)
  (abs (- x y)))

(defun triangular-dist (x y)
  (let ((d (linear-dist x y)))
    (abs (* d (1+ d) 1/2))))

(defun needed-fuel (crabs distfn)
  (let ((start (caar crabs))
        (end (caar (last crabs))))
    (loop for x from start to end
          minimizing (loop for crab in crabs
                           sum (* (cdr crab) (funcall distfn x (car crab)))))))

(defun part1 (input)
  (needed-fuel (read-crabs input) #'linear-dist))

(defun part2 (input)
  (needed-fuel (read-crabs input) #'triangular-dist))
