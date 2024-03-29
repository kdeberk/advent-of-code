(in-package :day3)

(defvar *input* (utils:read-lines "day3.txt"))
(defvar *test-input* (utils:read-lines "day3_test.txt"))

(defun string-to-bitvector (str)
  (coerce str 'list))

(defun bitvector-to-int (v)
  (parse-integer (coerce v 'string) :radix 2))

(defun most-common-bit (v)
  (if (<= (/ (length v) 2) (count #\1 v)) #\1 #\0))

(defun least-common-bit (v)
  (if (<= (/ (length v) 2) (count #\1 v)) #\0 #\1))

(defun part1 (input)
  (let* ((rows (mapcar #'string-to-bitvector input))
         (columns (apply #'mapcar #'list rows)))
    (* (bitvector-to-int (mapcar #'most-common-bit columns))
       (bitvector-to-int (mapcar #'least-common-bit columns)))))

(defun part2 (input)
  (labels ((filter (rows pos fn)
             (let* ((bit (funcall fn (mapcar (utils:partial #'nth pos) rows)))
                    (rows (remove-if-not (lambda (str) (char= bit (nth pos str))) rows)))
               (if (= 1 (length rows))
                   (first rows)
                   (filter rows (1+ pos) fn)))))
    (let ((rows (mapcar #'string-to-bitvector input)))
      (* (bitvector-to-int (filter rows 0 #'most-common-bit))
         (bitvector-to-int (filter rows 0 #'least-common-bit))))))

(define-test day3
  (is = 198 (part1 *test-input*))
  (is = 230 (part2 *test-input*)))
