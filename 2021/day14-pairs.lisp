(in-package :day14-pairs)

(defvar *test-input* (utils:read-input "day14_test.txt"))
(defvar *input* (utils:read-input "day14.txt"))

(defun read-puzzle (input)
  (destructuring-bind (start _ &rest steps) (str:split #\Newline input)
    (declare (ignore _))
    (list (coerce start 'list)
          (mapcar (lambda (step)
                    (destructuring-bind (pair insert) (str:split " -> " step)
                      (let ((a (char pair 0)) (b (char pair 1)) (c (char insert 0)))
                        (list (list a b) (list a c) (list c b)))))
                  steps))))

(defun count-letters (pair-counts)
  (let ((counts))
    (loop for ((a b) . count) in pair-counts
          do (progn (utils:push-or-inc a counts :by count)
                    (utils:push-or-inc b counts :by count)))
    counts))

(defun diff-most-and-least-common (letter-counts)
  (loop for (letter . count) in letter-counts
        minimize count into least-common
        maximize count into most-common
        finally (return (- most-common least-common))))

(defun solve (input n)
  (destructuring-bind (sequence steps) (read-puzzle input)
    (let ((pair-counts))
      (loop for (a b) on sequence
            when b
              do (utils:push-or-inc (list a b) pair-counts :test #'equal))
      (dotimes (_ n)
        (let ((next-counts))
          (loop for (pair . count) in pair-counts
                do (destructuring-bind (_ ac cb) (assoc pair steps :test #'equal)
                     (declare (ignore _))
                     (utils:push-or-inc ac next-counts :by count)
                     (utils:push-or-inc cb next-counts :by count))
                finally (setf pair-counts next-counts))))
      (let ((letter-counts (count-letters pair-counts)))
        (incf (cdr (assoc (car sequence) letter-counts)))
        (incf (cdr (assoc (car (last sequence)) letter-counts)))
        (/ (diff-most-and-least-common letter-counts) 2)))))

(defun part1 (input)
  (solve input 10))

(defun part2 (input)
  (solve input 40))

(define-test day14-pairs
  (is = 1588 (part1 *test-input*))
  (is = 2188189693529 (part2 *test-input*)))
