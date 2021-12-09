(in-package :day9)

(defvar *input* (utils:read-input "day9.txt"))
(defvar *test-input* (utils:read-input "day9_test.txt"))

(defvar *basin-wall* 9)

(defun read-grid (input)
  (let ((lines (str:split #\Newline input)))
    (make-array (list (length lines)
                      (length (first lines)))
                :initial-contents (mapcar (lambda (row)
                                            (map 'list (lambda (ch) (- (char-code ch) (char-code #\0))) row))
                                          lines))))

(defun part1 (input)
  (let ((grid (read-grid input)))
    (destructuring-bind (width height) (array-dimensions grid)
      (loop for x from 0 below width
            sum (loop for y from 0 below height
                      if (< (aref grid x y) (apply #'min (neighbors grid x y)))
                        sum (1+ (aref grid x y)))))))

(defun neighbors (grid x y)
  (destructuring-bind (width height) (array-dimensions grid)
    (remove-if-not #'identity
                   (mapcar (lambda (dx dy)
                             (let ((x (+ x dx)) (y (+ y dy)))
                               (when (and (< -1 x width) (< -1 y height))
                                 (aref grid x y))))
                           '(-1 0 0 1) '(0 -1 1 0)))))

(defun mark-basins (grid)
  (destructuring-bind (width height) (array-dimensions grid)
    ;; Give each cell a unique id.
    (loop for x from 0 below width
          do (loop for y from 0 below height
                   do (let ((cell (aref grid x y)))
                        (unless (= cell *basin-wall*)
                          (setf (aref grid x y) (+ (* 100 (1+ x)) y))))))
    ;; Some basic 'basin election', whereby we let each cell take over the lowest id.
    (loop
      (let (updated)
        (loop for x from 0 below width
              do (loop for y from 0 below height
                       do (utils:if-let (neighbors (remove *basin-wall* (neighbors grid x y)))
                            (let ((lowest-neighbor (apply #'min neighbors)))
                              (if (< lowest-neighbor (aref grid x y))
                                  (setf (aref grid x y) lowest-neighbor
                                        updated t))))))
        (unless updated
          (return-from mark-basins grid))))))

(defun count-basins (grid)
  (destructuring-bind (width height) (array-dimensions grid)
    (let ((basins))
      (loop for x from 0 below width
            do (loop for y from 0 below height
                     do (let ((cell (aref grid x y)))
                          (unless (= cell *basin-wall*)
                            (utils:if-let (basin (assoc cell basins))
                              (incf (cdr basin))
                              (push (cons cell 1) basins))))))
      basins)))

(defun part2 (input)
  (let ((grid (read-grid input)))
    (let ((counts (count-basins (mark-basins grid))))
      (apply #'* (subseq (sort (mapcar #'cdr counts) #'>) 0 3)))))

(define-test day9
  (is = 15 (part1 *test-input*))
  (is = 1134 (part2 *test-input*)))
