(in-package :day13)

(defvar *test-input* (utils:read-input "day13_test.txt"))
(defvar *input* (utils:read-input "day13.txt"))

(defun read-instructions (input)
  (destructuring-bind (points folds) (cl-ppcre:split "\\n\\n" input)
    (let ((points (mapcar (lambda (line)
                             (mapcar #'parse-integer (str:split "," line)))
                          (str:split #\Newline points)))
          (folds (mapcar (lambda (line)
                           (cl-ppcre:register-groups-bind (axis n)
                               ("fold along (x|y)=(\\d+)" line)
                             (list axis (when n (parse-integer n)))))
                         (str:split #\Newline folds))))
      (let ((grid (make-array (list (1+ (apply #'max (mapcar #'car points)))
                                    (1+ (apply #'max (mapcar #'cadr points))))
                              :initial-element nil)))
        (loop for (x y) in points
              do (setf (aref grid x y) t))
        (list grid folds)))))

(defun do-fold (grid fold)
  (destructuring-bind (width height) (array-dimensions grid)
    (destructuring-bind (axis n) fold
        (cond ((string= axis "x")
               (loop for d from 0 below n
                     do (loop for y from 0 below height
                              do (let ((dx (- n d 1)) (sx (+ n d 1)))
                                   (when (and (< -1 dx n) (< n sx width))
                                     (setf (aref grid dx y) (or (aref grid dx y) (aref grid sx y))))))
                     finally (return (adjust-array grid (list n height)))))
              ((string= axis "y")
               (loop for x from 0 below width
                     do (loop for d from 0 below n
                              do (let ((dy (- n d 1)) (sy (+ n d 1)))
                                   (when (and (< -1 dy n) (< n sy height))
                                     (setf (aref grid x dy) (or (aref grid x dy) (aref grid x sy))))))
                     finally (return-from do-fold (adjust-array grid (list width n)))))))))

(defun part1 (input)
  (destructuring-bind (grid folds) (read-instructions input)
    (let ((grid (do-fold grid (first folds))))
      (destructuring-bind (width height) (array-dimensions grid)
        (loop for x from 0 below width
              sum (loop for y from 0 below height
                        count (aref grid x y)))))))

(defun print-grid (grid)
  (destructuring-bind (width height) (array-dimensions grid)
    (loop for y from 0 below height
          do (loop for x from 0 below width
                   do (format t "~a" (if (aref grid x y) #\# #\Space))
                   finally (format t "~%")))))

(defun part2 (input)
  (destructuring-bind (grid folds) (read-instructions input)
    (print-grid (reduce #'do-fold folds :initial-value grid))))

(define-test day13
  (is = 17 (part1 *test-input*)))
