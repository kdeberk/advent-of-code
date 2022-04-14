(in-package :day18)

(defvar *test-input-1* (utils:read-lines "day18_test1.txt"))
(defvar *test-input-2* (utils:read-lines "day18_test2.txt"))
(defvar *test-input-3* (utils:read-lines "day18_test3.txt"))
(defvar *input* (utils:read-lines "day18.txt"))

(defstruct (snail-node (:print-function (lambda (node out depth)
                                          (declare (ignore depth))
                                          (with-slots (left right value) node
                                            (format out "~a~a~a"
                                                    (if (null left) "<" "")
                                                    (snail-node-value node)
                                                    (if (null right) ">" ""))))))
  left value right)

(defun join-nodes (first second)
  (when first
    (setf (snail-node-right first) second))
  (when second
    (setf (snail-node-left second) first)))

(defun read-snailfish-number (string)
  (labels ((read-number (string)
             (read-from-string (reduce (lambda (string pair)
                                         (destructuring-bind (replace with) pair
                                           (substitute with replace string)))
                                       '((#\Comma #\Space) (#\[ #\() (#\] #\)))
                                       :initial-value string)))
           (traverse (cur &optional prev)
             (if (integerp cur)
                 (let ((cur (make-snail-node :left prev :value cur)))
                   (join-nodes prev cur)
                   (list cur cur))
                 (destructuring-bind (left right) cur
                   (destructuring-bind (left prev) (traverse left prev)
                     (destructuring-bind (right prev) (traverse right prev)
                       (list (list left right) prev)))))))
    (let ((number (read-number string)))
      (first (traverse number)))))

(defun snail-reduce (number)
  (labels ((explode (pair)
             (destructuring-bind (left right) pair
               (let ((new (make-snail-node :value 0)))
                 (with-slots (left value) left
                   (when left
                     (incf (snail-node-value left) value)))
                 (with-slots (right value) right
                   (when right
                     (incf (snail-node-value right) value)))
                 (join-nodes (snail-node-left left) new)
                 (join-nodes new (snail-node-right right))
                 new)))
           (split (node)
             (with-slots (value) node
               (let ((left (make-snail-node :value (floor (/ value 2))))
                     (right (make-snail-node :value (ceiling (/ value 2)))))
                 (join-nodes (snail-node-left node) left)
                 (join-nodes left right)
                 (join-nodes right (snail-node-right node))
                 (list left right))))
           (reduce-step (number)
             (labels ((traverse-explodes (pair &optional (depth 0))
                        (let ((left (first pair)) (right (second pair)) (modified))
                          (when (listp left)
                            (destructuring-bind (new-left modified-left) (traverse-explodes left (1+ depth))
                              (setf left new-left
                                    modified modified-left)))
                          (when (and (not modified) (listp right))
                            (destructuring-bind (new-right modified-right) (traverse-explodes right (1+ depth))
                              (setf right new-right
                                   modified modified-right)))
                          (if (and (not modified) (<= 4 depth))
                              (list (explode pair) t)
                              (list (list left right) modified))))
                      (traverse-splits (cur)
                        (cond ((listp cur)
                               (let ((left (first cur)) (right (second cur)) modified)
                                 (destructuring-bind (new-left modified-left) (traverse-splits left)
                                   (setf left new-left
                                         modified modified-left))
                                 (unless modified
                                   (destructuring-bind (new-right modified-right) (traverse-splits right)
                                     (setf right new-right
                                           modified modified-right)))
                                 (list (list left right) modified)))
                              ((<= 10 (snail-node-value cur))
                               (list (split cur) t))
                              (t
                               (list cur nil)))))
               (destructuring-bind (number exploded) (traverse-explodes number)
                 (if exploded
                     (list number exploded)
                     (traverse-splits number))))))
    (loop
      (destructuring-bind (new-number modified) (reduce-step number)
        (if modified
            (setf number new-number)
            (return new-number))))))

(defun snail-merge (a b)
  (let ((last-a (let ((node a)) (loop if (listp node)
                                        do (setf node (car (last node)))
                                      else
                                        do (return node))))
        (first-b (let ((node b)) (loop if (listp node)
                                         do (setf node (car node))
                                       else
                                         do (return node)))))
    (join-nodes last-a first-b)
    (snail-reduce (list a b))))

(defun magnitude (node)
  (if (listp node)
      (+ (* 3 (magnitude (first node)))
         (* 2 (magnitude (second node))))
      (snail-node-value node)))

(defun part1 (numbers)
  (magnitude (reduce (lambda (acc cur)
                       (snail-merge acc cur))
                     (mapcar #'read-snailfish-number numbers))))

(defun part2 (input)
  (loop for a in input
        maximizing (loop for b in input
                         if (not (string= a b))
                           maximizing (magnitude (snail-merge (read-snailfish-number a) (read-snailfish-number b))))))
