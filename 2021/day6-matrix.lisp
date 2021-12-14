(in-package :day6-matrix)

(defvar *test-input* (utils:read-numbers "day6_test.txt"))
(defvar *input* (utils:read-numbers "day6.txt"))

(defvar *laternfish-matrix* #2A((0 1 0 0 0 0 0 0 0)
                                (0 0 1 0 0 0 0 0 0)
                                (0 0 0 1 0 0 0 0 0)
                                (0 0 0 0 1 0 0 0 0)
                                (0 0 0 0 0 1 0 0 0)
                                (0 0 0 0 0 0 1 0 0)
                                (1 0 0 0 0 0 0 1 0)
                                (0 0 0 0 0 0 0 0 1)
                                (1 0 0 0 0 0 0 0 0)))

(defun count-fishes (numbers)
  (let ((counts (loop for n from 0 to 8 collect (cons n 0))))
    (dolist (n numbers)
      (incf (cdr (assoc n counts))))
    (mapcar #'cdr (sort counts #'< :key #'car))))

(defun grow-lanternfish (counts days)
  (let ((v (matrix:*-vector (matrix:exponent *laternfish-matrix* days)
                            (make-array 9 :initial-contents counts))))
    (apply #'+ (coerce v 'list))))

(defun part1 (input)
  (grow-lanternfish (count-fishes input) 80))

(defun part2 (input)
  (grow-lanternfish (count-fishes input) 256))

(define-test day6-matrix
  (is = 5934 (part1 *test-input*))
  (is = 26984457539 (part2 *test-input*)))
