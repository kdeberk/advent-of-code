(defpackage #:day2
  (:use :cl))
(in-package :day2)

(defun read-instructions (input)
  (mapcar (lambda (line)
            (destructuring-bind (dir dist) (str:split #\space line)
              (list dir (parse-integer dist))))
          (str:split #\newline input)))

(defstruct (submarine (:conc-name nil)) depth horizontal aim)

(defun part1 (input)
  (let ((sub (make-submarine :depth 0 :horizontal 0)))
    (dolist (instr (read-instructions input))
      (destructuring-bind (dir dist) instr
        (cond
          ((string= dir "forward") (incf (horizontal sub) dist))
          ((string= dir "down") (incf (depth sub) dist))
          ((string= dir "up") (decf (depth sub) dist)))))
    (* (depth sub) (horizontal sub))))

(defun part2 (input)
  (let ((sub (make-submarine :depth 0 :horizontal 0 :aim 0)))
    (dolist (instr (read-instructions input))
      (destructuring-bind (dir x) instr
        (cond
          ((string= dir "forward") (setf (horizontal sub) (+ (horizontal sub) x)
                                         (depth sub) (+ (depth sub) (* (aim sub) x))))
          ((string= dir "down") (incf (aim sub) x))
          ((string= dir "up") (decf (aim sub) x)))))
    (* (depth sub) (horizontal sub))))
