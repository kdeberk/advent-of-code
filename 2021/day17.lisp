(in-package :day17)

(defstruct target min-x min-y max-x max-y)

(defvar *test-input* (make-target :min-x 20 :max-x 30 :min-y -10 :max-y -5))
(defvar *input* (make-target :min-x 155 :max-x 182 :min-y -117 :max-y -67))

(defun calc-trajectory (vx vy target)
  (with-slots (min-x min-y max-x max-y) target
    (let ((cur-x 0) (cur-y 0)
          (height 0))
      (loop
        (cond ((or (< max-x cur-x) (< cur-y min-y)) ;; to the right of x or below y
               (return nil))
              ((and (<= min-x cur-x max-x) (<= min-y cur-y max-y))
               (return height))
              (t (setf cur-x (+ cur-x vx)
                       cur-y (+ cur-y vy)
                       vx (max 0 (- vx 1))
                       vy (- vy 1)
                       height (max height cur-y))))))))

(defun part1 (input)
  (loop for vx from 1 to 200
        maximizing (loop for vy from -200 to 200
                         maximizing (or (calc-trajectory vx vy input) 0))))

(defun part2 (input)
  (loop for vx from 1 to 200
        sum (loop for vy from -200 to 200
                  count (calc-trajectory vx vy input))))

(define-test day17
  (is = 45 (part1 *test-input*))
  (is = 112 (part2 *test-input*)))
