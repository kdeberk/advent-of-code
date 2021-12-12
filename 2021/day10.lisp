(in-package :day10)

(defvar *input* (utils:read-lines "day10.txt"))
(defvar *test-input* (utils:read-lines "day10_test.txt"))

(defvar *pairs* '((#\( . #\)) (#\[ . #\]) (#\{ . #\}) (#\< . #\>)))
(defvar *part1-scores* '((#\) . 3) (#\] . 57) (#\} . 1197) (#\> . 25137)))
(defvar *part2-scores* '((#\) . 1) (#\] . 2) (#\} . 3) (#\> . 4)))

(defun get-score (line)
  (let ((stack))
    (dolist (ch (coerce line 'list))
      (let ((closing (assoc ch *pairs*)))
        (cond (closing
               (push (cdr closing) stack))
              ((not (eq ch (pop stack)))
               (return-from get-score (cdr (assoc ch *part1-scores*)))))))
    (values 0 stack)))

(defun part1 (input)
  (loop for line in input
        sum (get-score line)))

(defun part2 (input)
  (labels ((median (l)
             (let ((l (sort l #'<)))
               (nth (round (/ (length l) 2)) l)))
           (stack-score (stack)
             (reduce (lambda (acc cur)
                       (+ (* 5 acc) (cdr (assoc cur *part2-scores*))))
                     stack
                     :initial-value 0)))
    (loop for line in input
          collect (multiple-value-bind (score stack) (get-score line)
                    (when (= 0 score)
                      (stack-score stack))) into scores
          finally (return (median (remove nil scores))))))

(define-test day10
  (is = 26397 (part1 *test-input*))
  (is = 323613 (part1 *input*))
  (is = 288957 (part2 *test-input*))
  (is = 3103006161 (part2 *input*)))
