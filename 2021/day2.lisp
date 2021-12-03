(in-package :day2)

(defvar *input* (read-lines "day2.txt"))
(defvar *test-input* (read-lines "day2_test.txt"))

(defun parse-instructions (input)
  (mapcar (lambda (line)
            (destructuring-bind (dir dist) (str:split #\space line)
              (list dir (parse-integer dist))))
          input))

(defstruct (submarine (:conc-name nil)) depth horizontal aim)

(defun part1 (input)
  (let ((sub (make-submarine :depth 0 :horizontal 0)))
    (dolist (instr (parse-instructions input))
      (destructuring-bind (dir dist) instr
        (stringcase dir
          ("forward" (incf (horizontal sub) dist))
          ("down" (incf (depth sub) dist))
          ("up" (decf (depth sub) dist)))))
    (* (depth sub) (horizontal sub))))

(defun part2 (input)
  (let ((sub (make-submarine :depth 0 :horizontal 0 :aim 0)))
    (dolist (instr (parse-instructions input))
      (destructuring-bind (dir x) instr
        (stringcase dir
          ("forward" (setf (horizontal sub) (+ (horizontal sub) x)
                           (depth sub) (+ (depth sub) (* (aim sub) x))))
          ("down" (incf (aim sub) x))
          ("up" (decf (aim sub) x)))))
    (* (depth sub) (horizontal sub))))
