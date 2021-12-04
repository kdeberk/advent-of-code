(in-package #:day1)

(defvar *input* (utils:read-numbers "day1.txt"))
(defvar *test-input* (utils:read-numbers "day1_test.txt"))

(defun part1 (input)
  (loop for (a b) on input
        count (and b (< a b))))

(defun part2 (input)
  (part1 (loop for (a b c) on input
               if (and b c) collect (+ a b c))))
