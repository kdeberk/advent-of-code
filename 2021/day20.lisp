(in-package :day20)

(defvar *test-input* (utils:read-input "day20_test.txt"))
(defvar *input* (utils:read-input "day20.txt"))

(defun grow-grid (rule grid step)
  (destructuring-bind (width height) (array-dimensions grid)
    (labels ((cell (x y)
               (let ((ch (cond ((and (< -1 x width) (< -1 y height))
                                (aref grid x y))
                               ((= step 0)
                                #\.)
                               ((= 0 (mod step 2))
                                (char rule (1- (length rule)))
                                )
                               (t
                                (char rule 0)))))
                 (if (char= #\# ch) #\1 #\0))))
      (let ((output (make-array (list (+ 2 width) (+ 2 height)))))
        (loop for x from -1 below (1+ width)
              do (loop for y from -1 below (1+ height)
                       do (let ((key (loop for xx from (1- x) to (1+ x)
                                            appending (loop for yy from (1- y) to (1+ y)
                                                            collecting (cell xx yy)))))
                            (let ((key (parse-integer (coerce key 'string) :radix 2)))
                              (setf (aref output (1+ x) (1+ y)) (char rule key))))))
        output))))

(defun count-cells (grid)
  (destructuring-bind (width height) (array-dimensions grid)
    (loop for x from 0 below width
          summing (loop for y from 0 below height
                        counting (char= #\# (aref grid x y))))))

(defun enhance (rule grid steps)
  (dotimes (i steps)
    (setf grid (grow-grid rule grid i)))
  grid)

(defun part1 (input)
  (destructuring-bind (rule grid) (cl-ppcre:split "\\n\\n" input)
    (let ((grid (utils:read-grid grid :char)))
      (count-cells (enhance rule grid 2)))))

(defun part2 (input)
  (destructuring-bind (rule grid) (cl-ppcre:split "\\n\\n" input)
    (let ((grid (utils:read-grid grid :char)))
      (count-cells (enhance rule grid 50)))))

;; TODO: test input is still failing
