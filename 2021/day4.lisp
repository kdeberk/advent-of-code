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

(defun cross-out (n board)
  (loop for i from 0 below 5
        do (loop for j from 0 below 5
                 when (eq n (aref board i j))
                   do (setf (aref board i j) nil)))
  board)

(defun bingo? (board)
  (labels ((crossed-out-rowcol? (getcell)
             (loop for i from 0 below 5
                   always (null (funcall getcell i)))))
    (loop for i from 0 below 5
          if (or (crossed-out-rowcol? (lambda (row) (aref board row i)))
                 (crossed-out-rowcol? (lambda (col) (aref board i col))))
            return t)))

(defun score (board)
  (loop for i from 0 below 5
        sum (loop for j from 0 below 5
                  sum (or (aref board i j) 0))))

(defun part1 (input)
  (destructuring-bind (numbers boards) (read-boards input)
    (loop for n in numbers
          do (loop for board in boards
                   do (progn
                        (cross-out n board)
                        (if (bingo? board)
                            (return-from part1 (* n (score board)))))))))

(defun part2 (input)
  (destructuring-bind (numbers boards) (read-boards input)
    (loop for n in numbers
          do (let* ((open (remove-if #'bingo? (mapcar (utils:partial #'cross-out n) boards))))
               (when (not open)
                 (return-from part2 (* n (score (first boards)))))
               (setf boards open)))))
