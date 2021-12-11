(in-package :day11)

(defvar *input* (utils:read-input "day11.txt"))
(defvar *test-input* (utils:read-input "day11_test.txt"))

(defun flash-neighbors (grid x y)
  (destructuring-bind (width height) (array-dimensions grid)
    (let ((neighbors (mapcar (lambda (dx dy)
                               (list (+ x dx) (+ y dy)))
                             (list -1 -1 -1  0  0  1  1  1)
                             (list -1  0  1 -1  1 -1  0  1))))
      (let ((flashed))
        (dolist (neighbor neighbors)
          (destructuring-bind (x y) neighbor
            (when (and (< -1 x width) (< -1 y height)
                       (< 0 (aref grid x y))) ;; Neighbor did not flash this round
              (incf (aref grid x y))
              (when (< 9 (aref grid x y)) ;; Neighbor will flash this round
                (push neighbor flashed)))))
        flashed))))

(defun flash-iter (grid)
  (destructuring-bind (width height) (array-dimensions grid)
    (let* ((flashing)
           (flash-count 0))
      (loop for x from 0 below width
            do (loop for y from 0 below height
                     do (progn
                          (incf (aref grid x y))
                          (when (< 9 (aref grid x y))
                            (push (list x y) flashing)))))
      (loop
        (when (not flashing)
          (return-from flash-iter flash-count))
        (destructuring-bind (x y) (pop flashing)
          (when (< 9 (aref grid x y))
            (setf (aref grid x y) 0
                  flash-count (1+ flash-count)
                  flashing (append (flash-neighbors grid x y) flashing))))))))

(defun part1 (input)
  (let ((grid (utils:read-grid input :integer))
        (flash-count 0))
    (dotimes (_ 100)
      (incf flash-count (flash-iter grid)))
    flash-count))

(defun part2 (input)
  (let ((grid (utils:read-grid input :integer))
        (iter 0))
    (loop
      (when (= 100 (flash-iter grid))
        (return (1+ iter)))
      (incf iter))))

(define-test day11
  (is = 1656 (part1 *test-input*))
  (is = 195 (part2 *test-input*)))
