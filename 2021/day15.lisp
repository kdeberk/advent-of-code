(in-package :day15)

(setf *test-input* (utils:read-grid (utils:read-input "day15_test.txt") :integer))
(setf *input* (utils:read-grid (utils:read-input "day15.txt") :integer))

(defvar *max-cell* (1- (expt 2 64)))

(defun walk-path (grid)
  (destructuring-bind (width height) (array-dimensions grid)
    (let ((best (make-array (array-dimensions grid) :initial-element *max-cell*)))
      (labels ((cell (x y)
                 (if (and (< -1 x width) (< -1 y height))
                     (aref best x y)
                     *max-cell*))
               (dyn-pass (start-x start-y)
                 (let ((improved))
                   (loop for x from start-x below width
                         do (loop for y from start-y below height
                                  do (let ((cur (aref best x y))
                                           (calc (+ (aref grid x y) (min (cell (1- x) y)
                                                                         (cell (1+ x) y)
                                                                         (cell x (1- y))
                                                                         (cell x (1+ y))))))
                                       (when (and (< calc cur) (< calc (aref best (1- width) (1- height))))
                                         (setf (aref best x y) calc
                                               improved t)))))
                   improved)))
        (setf (aref best 0 0) 0)
        (loop for i from 0 below width
              unless (dyn-pass i i)
                do (return)))
      (aref best (1- width) (1- height)))))

(defun embiggen (smaller)
  (destructuring-bind (width height) (array-dimensions smaller)
    (let ((bigger (make-array (list (* 5 width) (* 5 height)))))
      (loop for grid-x from 0 below 5
            do (loop for grid-y from 0 below 5
                     do (loop for cell-x from 0 below width
                              do (loop for cell-y from 0 below height
                                       do (progn
                                            (setf (aref bigger (+ (* width grid-x) cell-x) (+ (* height grid-y) cell-y))
                                                       (let ((val (+ (aref smaller cell-x cell-y)
                                                                     grid-x grid-y)))
                                                         (loop
                                                           (when (< 0 val 10)
                                                             (return val))
                                                           (setf val (- val 9))))))))))
      bigger)))

(defun part1 (input)
  (walk-path input))

(defun part2 (input)
  (walk-path (embiggen input)))
