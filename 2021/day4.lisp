(in-package :day4)

(defvar *input* (utils:read-input "day4.txt"))
(defvar *test-input* (utils:read-input "day4_test.txt"))

(defun read-boards (input)
  (destructuring-bind (numbers &rest boards) (cl-ppcre:split "\\n\\n" input)
    (list (mapcar #'parse-integer (str:split "," numbers))
          (mapcar (lambda (board)
                    (let* ((board (str:replace-all '(#\Newline) ") (" board))
                           (board (concatenate 'string "((" board "))")))
                      (make-array '(5 5) :initial-contents (read-from-string board))))
                  boards))))

(defun cross-out (board n)
  (dotimes (i 5)
    (dotimes (j 5)
      (when (eq n (aref board i j))
        (setf (aref board i j) nil)))))

(defun bingo? (board)
  (labels ((crossed-out-rowcol? (getcell)
             (loop for i from 0 below 5
                   if (funcall getcell i) return nil
                     finally (return t))))
    (loop for i from 0 below 5
          if (or (crossed-out-rowcol? (lambda (row) (aref board row i)))
                 (crossed-out-rowcol? (lambda (col) (aref board i col))))
            return t)))

(defun score (board)
  (let ((sum 0))
    (dotimes (i 5)
      (dotimes (j 5)
        (incf sum (or (aref board i j) 0))))))

(defun part1 (input)
  (destructuring-bind (numbers boards) (read-boards input)
    (dolist (n numbers)
      (dolist (board boards)
        (cross-out board n)
        (if (bingo? board)
            (return-from part1 (* n (score board))))))))

(defun part2 (input)
  (destructuring-bind (numbers boards) (read-boards input)
    (dolist (n numbers)
      (dolist (board boards)
        (cross-out board n))
      (let ((open (remove-if #'bingo? boards)))
        (when (not open)
          (return-from part2 (* n (score (first boards)))))
        (setf boards open)))))
