(in-package :day5)

(defvar *input* (utils:read-lines "day5.txt"))
(defvar *test-input* (utils:read-lines "day5_test.txt"))

(defstruct line sx sy dx dy)

(defun read-lines (input)
  (mapcar (lambda (line)
            (destructuring-bind (sx sy dx dy) (mapcar #'parse-integer (cl-ppcre:split "[^0-9]+" line))
              (make-line :sx sx :sy sy :dx dx :dy dy)))
          input))

(defmethod horizontal? ((line line))
  (= (line-sy line) (line-dy line)))

(defmethod vertical? ((line line))
  (= (line-sx line) (line-dx line)))

(defun make-grid (lines)
  (let ((maxx (apply #'max (append (mapcar #'line-sx lines)
                                   (mapcar #'line-dx lines))))
        (maxy (apply #'max (append (mapcar #'line-sy lines)
                                   (mapcar #'line-dy lines)))))
    (make-array (list (1+ maxx) (1+ maxy)) :element-type 'integer)))

(defun count-overlaps (grid)
  (destructuring-bind (width height) (array-dimensions grid)
    (loop for x from 0 below width
          sum (loop for y from 0 below height
                    count (< 1 (aref grid x y))))))

(defun trace-line (grid line)
  (with-slots (sx sy dx dy) line
    (labels ((Δ (s d)
               (cond ((< s d) #'1+)
                     ((< d s) #'1-)
                     (t #'identity))))
      (let ((x sx) (y sy)
            (Δx (Δ sx dx))
            (Δy (Δ sy dy)))
        (loop
          (incf (aref grid x y))
          (if (and (= x dx) (= y dy))
              (return)
              (setf x (funcall Δx x) y (funcall Δy y))))))))

(defun part1 (input)
  (let* ((lines (read-lines input))
         (grid (make-grid lines)))
    (dolist (line lines)
      (when (or (horizontal? line) (vertical? line))
        (trace-line grid line)))
    (count-overlaps grid)))

(defun part2 (input)
  (let* ((lines (read-lines input))
         (grid (make-grid lines)))
    (dolist (line lines)
      (trace-line grid line))
    (count-overlaps grid)))
