(in-package :matrix)

(defun exponent (a n)
  (if (= 1 n)
      a
      (let ((b (exponent a (ash n -1))))
        (let ((b (*-matrix b b)))
          (if (= 0 (mod n 2))
              b
              (*-matrix b a))))))

(defun *-matrix (a b)
  (let ((c (make-array '(9 9) :element-type 'integer)))
    (loop for i from 0 below 9
          do (loop for j from 0 below 9
                   do (loop for k from 0 below 9
                            do (incf (aref c i j) (* (aref a i k) (aref b k j))))))
    c))

(defun *-vector (m v)
  (let ((vp (make-array 9 :element-type 'integer)))
    (loop for i from 0 below 9
          do (loop for j from 0 below 9
                   do (incf (aref vp i) (* (aref m i j) (aref v j)))))
    vp))
