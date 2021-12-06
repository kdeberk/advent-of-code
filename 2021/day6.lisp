(in-package :day6)

(defvar *test-input* (utils:read-numbers "day6_test.txt"))
(defvar *input* (utils:read-numbers "day6.txt"))

(defun count-occurrences (numbers)
  (let ((occurrences (loop for n from 0 to 8 collect (cons n 0))))
    (dolist (n numbers)
      (incf (cdr (assoc n occurrences))))
    (mapcar #'cdr (sort occurrences #'< :key #'car))))

(defun grow-lanternfish (occurrences days)
  (setf (cdr (last occurrences)) occurrences) ;; Make it cyclic
  (dotimes (day days)
    (let ((spawning (first occurrences)))
      (setf occurrences (cdr occurrences))
      (incf (nth 6 occurrences) spawning)))
  (apply #'+ (subseq occurrences 0 9)))

(defun part1 (input)
  (grow-lanternfish (count-occurrences input) 80))

(defun part2 (input)
  (grow-lanternfish (count-occurrences input) 256))

;; Aternative form using matrix multiplications

(setf *laternfish-matrix* #2A((0 1 0 0 0 0 0 0 0)
                              (0 0 1 0 0 0 0 0 0)
                              (0 0 0 1 0 0 0 0 0)
                              (0 0 0 0 1 0 0 0 0)
                              (0 0 0 0 0 1 0 0 0)
                              (0 0 0 0 0 0 1 0 0)
                              (1 0 0 0 0 0 0 1 0)
                              (0 0 0 0 0 0 0 0 1)
                              (1 0 0 0 0 0 0 0 0)))

(defun grow-lanternfish-alt (occurrences days)
  (let ((v (matrix:*-vector (matrix:exponent *laternfish-matrix* days)
                            (make-array 9 :initial-contents occurrences))))
    (apply #'+ (coerce v 'list))))

(defun part1-alt (input)
  (grow-lanternfish-alt (count-occurrences input) 80))

(defun part2-alt (input)
  (grow-lanternfish-alt (count-occurrences input) 256))

(define-test day6
  (is = 5934 (part1 *test-input*))
  (is = 5934 (part1-alt *test-input*))
  (is = 26984457539 (part2 *test-input*))
  (is = 26984457539 (part2-alt *test-input*)))
