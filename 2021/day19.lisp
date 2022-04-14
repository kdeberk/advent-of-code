(in-package :day19)

(defvar *test-input* (utils:read-input "day19_test.txt"))
(defvar *input* (utils:read-input "day19.txt"))

(defvar *rotations* (mapcar (lambda (m)
                              (utils:partial #'matrix:*-vector (make-array '(3 3) :initial-contents m)))
                            '((( 1  0  0) ( 0  1  0) ( 0  0  1)) (( 1  0  0) ( 0  0 -1) ( 0  1  0)) (( 1  0  0) ( 0 -1  0) ( 0  0 -1)) (( 1  0  0) ( 0  0  1) ( 0 -1  0))
                              (( 0 -1  0) ( 1  0  0) ( 0  0  1)) (( 0  0  1) ( 1  0  0) ( 0  1  0)) (( 0  1  0) ( 1  0  0) ( 0  0 -1)) (( 0  0 -1) ( 1  0  0) ( 0 -1  0))
                              ((-1  0  0) ( 0 -1  0) ( 0  0  1)) ((-1  0  0) ( 0  0 -1) ( 0 -1  0)) ((-1  0  0) ( 0  1  0) ( 0  0 -1)) ((-1  0  0) ( 0  0  1) ( 0  1  0))
                              (( 0  1  0) (-1  0  0) ( 0  0  1)) (( 0  0  1) (-1  0  0) ( 0 -1  0)) (( 0 -1  0) (-1  0  0) ( 0  0 -1)) (( 0  0 -1) (-1  0  0) ( 0  1  0))
                              (( 0  0 -1) ( 0  1  0) ( 1  0  0)) (( 0  1  0) ( 0  0  1) ( 1  0  0)) (( 0  0  1) ( 0 -1  0) ( 1  0  0)) (( 0 -1  0) ( 0  0 -1) ( 1  0  0))
                              (( 0  0 -1) ( 0 -1  0) (-1  0  0)) (( 0 -1  0) ( 0  0  1) (-1  0  0)) (( 0  0  1) ( 0  1  0) (-1  0  0)) (( 0  1  0) ( 0  0 -1) (-1  0  0)))))

(defun distance (a b)
  (loop for i from 0 below 3
        sum (abs (- (aref a i) (aref b i)))))

(defstruct scanner pos beacons scanners)
(defstruct beacon pos distances)

(defun read-scanners (input)
  (mapcar (lambda (scan)
            (let ((beacons (mapcar (lambda (line)
                                     (make-beacon :pos (make-array 3 :initial-contents (mapcar #'parse-integer (str:split #\Comma line)))))
                                   (rest (str:split #\Newline scan)))))
              (loop for a in beacons
                    do (loop for b in beacons
                             do (let ((d (distance (beacon-pos a) (beacon-pos b))))
                                  (when (< 0 d)
                                    (push d (beacon-distances a))))))
              (make-scanner :beacons beacons)))
          (cl-ppcre:split "\\n\\n" input)))

(defun overlap-scanners (scanner-a scanner-b)
  (loop for beacon-a in (scanner-beacons scanner-a)
        do (loop for beacon-b in (scanner-beacons scanner-b)
                 do (when (<= 11 (length (intersection (beacon-distances beacon-a) (beacon-distances beacon-b))))
                      (loop for rotation in *rotations*
                            do (let ((beacons-a-pos (mapcar #'beacon-pos (scanner-beacons scanner-a)))
                                     (beacons-b-pos (mapcar rotation (mapcar #'beacon-pos (scanner-beacons scanner-b))))
                                     (d (matrix:--vector (beacon-pos beacon-a) (funcall rotation (beacon-pos beacon-b)))))
                                 (let ((beacons-b-pos (mapcar (lambda (p) (matrix:+-vector p d)) beacons-b-pos)))
                                   (when (<= 12 (length (intersection beacons-a-pos beacons-b-pos :test #'equalp)))
                                     (let ((merged-beacons (mapcar (lambda (p)
                                                                     (make-beacon :pos p))
                                                                   (remove-duplicates (append beacons-a-pos beacons-b-pos)
                                                                                      :test #'equalp))))
                                       (loop for a in merged-beacons
                                             do (loop for b in merged-beacons
                                                      do (let ((d (distance (beacon-pos a) (beacon-pos b))))
                                                           (when (< 0 d)
                                                             (push d (beacon-distances a))))))

                                       (return-from overlap-scanners (make-scanner :beacons merged-beacons
                                                                                   :scanners (append (scanner-scanners scanner-a)
                                                                                                     (list d)))))))))))))


(defun overlap-all-scanners (scanners)
  (let ((fixed (car scanners))
        (scanners (cdr scanners)))
    (push (make-array 3 :initial-element 0) (scanner-scanners fixed))
    (loop
      unless scanners
        return fixed
      do (let (next)
           (dolist (scanner scanners)
             (let ((overlap (overlap-scanners fixed scanner)))
               (if overlap
                   (setf fixed overlap)
                   (push scanner next))))
           (setf scanners next)))))

(defun part1 (input)
  (let ((overlapped (overlap-all-scanners (read-scanners input))))
    (length (scanner-beacons overlapped))))

(defun part2 (input)
  (let ((overlapped (overlap-all-scanners (read-scanners input))))
    (loop for pos-a in (scanner-scanners overlapped)
          maximize (loop for pos-b in (scanner-scanners overlapped)
                         maximize (distance pos-a pos-b)))))
