(in-package :day25)

(defvar *test-input* (utils:read-grid (utils:read-input "day25_test.txt") :char))
(defvar *input* (utils:read-grid (utils:read-input "day25.txt") :char))

(defun move-herds (grid)
  (destructuring-bind (height width) (array-dimensions grid)
    (let (moved)
      (labels ((move (ch dy dx)
                 (let ((copy (make-array (array-dimensions grid) :initial-element #\.)))
                   (loop for x from 0 below width
                         do (loop for y from 0 below height
                                  do (cond ((char= ch (aref grid y x))
                                            (let ((next-x (mod (+ x dx) width))
                                                  (next-y (mod (+ y dy) height)))
                                              (if (char= #\. (aref grid next-y next-x))
                                                  (setf (aref copy next-y next-x) ch
                                                        moved t)
                                                  (setf (aref copy y x) ch))))
                                           ((char= #\. (aref copy y x))
                                            (setf (aref copy y x) (aref grid y x))))))
                   (setf grid copy))))
        (move #\> 0 1)
        (move #\v 1 0)
        (list moved grid)))))

(defun part1 (input)
  (let ((grid input))
    (loop for i from 0
          do (destructuring-bind (moved new-grid) (move-herds grid)
               (unless moved
                 (return (1+ i)))
               (setf grid new-grid)))))

(define-test day25
  (is = 58 (part1 *test-input*))
  (is = 486 (part1 *input*)))
