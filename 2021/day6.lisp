(in-package :day6)

(defvar *test-input* (utils:read-numbers "day6_test.txt"))
(defvar *input* (utils:read-numbers "day6.txt"))

(defun count-occurrences (numbers)
  (let ((occurrences (loop for n from 0 to 8 collect (cons n 0))))
    (dolist (n numbers)
      (incf (cdr (assoc n occurrences))))
    (mapcar #'cdr (sort occurrences #'< :key #'car))))

(defun grow-lanternfish (occurrences days)
  (dotimes (day days)
    (let ((spawning (first occurrences)))
      (setf occurrences (append (rest occurrences) (list spawning)))
      (incf (nth 6 occurrences) spawning)))
  (apply #'+ occurrences))

(defun part1 (input)
  (grow-lanternfish (count-occurrences input) 80))

(defun part2 (input)
  (grow-lanternfish (count-occurrences input) 256))

(define-test day6
  (is = 5934 (part1 *test-input*))
  (is = 26984457539 (part2 *test-input*)))
