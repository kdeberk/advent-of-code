(in-package :day6)

(defvar *test-input* (utils:read-numbers "day6_test.txt"))
(defvar *input* (utils:read-numbers "day6.txt"))

(defun count-fishes (numbers)
  (let ((counts (loop for n from 0 to 8 collect (cons n 0))))
    (dolist (n numbers)
      (incf (cdr (assoc n counts))))
    (mapcar #'cdr (sort counts #'< :key #'car))))

(defun grow-lanternfish (counts days)
  (setf (cdr (last counts)) counts) ;; Make it cyclic
  (dotimes (day days)
    (let ((spawning (first counts)))
      (setf counts (cdr counts))
      (incf (nth 6 counts) spawning)))
  (apply #'+ (subseq counts 0 9)))

(defun part1 (input)
  (grow-lanternfish (count-fishes input) 80))

(defun part2 (input)
  (grow-lanternfish (count-fishes input) 256))

(define-test day6
  (is = 5934 (part1 *test-input*))
  (is = 26984457539 (part2 *test-input*)))
