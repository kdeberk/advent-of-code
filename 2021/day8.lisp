(in-package :day8)

(defvar *test-input* (utils:read-lines "day8_test.txt"))
(defvar *input* (utils:read-lines "day8.txt"))

(defvar *all-segments* (coerce "abcdefg" 'list))

(defun part1 (input)
  (apply #'+ (mapcar (lambda (line)
                        (let ((signals (cl-ppcre:split "[^a-z]+" line)))
                          (loop for signal in (last signals 4)
                                count (member (length signal) '(2 3 4 7)))))
                     input)))

(defun part2 (input)
  (apply #'+ (mapcar #'solve-line input)))

(defun solve-line (line)
  (let* ((signals (mapcar (lambda (signal) (coerce signal 'list))
                          (cl-ppcre:split "[^a-z]+" line)))
         (display-signals (last signals 4)))
    (let ((mapping (solve signals)))
      (to-int (mapcar (utils:partial #'to-digit mapping)
                      display-signals)))))

(defun solve (signals)
  (let ((mapping (initial-mapping)))
    ;; First, narrow the candidates by using the segment lengths
    (labels ((narrow (keys chars)
               (dolist (key keys)
                 (setf (cdr (assoc key mapping)) (intersection chars (rest (assoc key mapping)))))))
      (dolist (signal signals)
        (case (length signal)
          (2 (narrow signal '(#\c #\f)))
          (3 (narrow signal '(#\a #\c #\f)))
          (4 (narrow signal '(#\b #\c #\d #\f)))
          (6 (narrow (set-difference *all-segments* signal) '(#\c #\d #\e))))))
    ;; For each deduced mapping, remove the segment from other the mappings until all 7 segments have been deduced.
    (let ((solution))
      (dotimes (_ 7)
        (destructuring-bind (key segment) (find 2 mapping :key #'length)
          (push (cons key segment) solution)
          (dolist (m mapping)
            (setf (rest m) (remove segment (rest m))))))
      solution)))

(defun initial-mapping ()
  (mapcar (lambda (ch) (cons ch *all-segments*)) *all-segments*))

(defun to-digit (mapping signal)
  (let ((mapped (mapcar (lambda (c)
                          (cdr (assoc c mapping :test #'char=)))
                        signal)))
    (position (coerce (sort mapped #'< :key #'char-code) 'string)
              (list "abcefg" "cf" "acdeg" "acdfg" "bcdf" "abdfg" "abdefg" "acf" "abcdefg" "abcdfg")
              :test #'string=)))

(defun to-int (digits)
  (reduce (lambda (acc cur) (+ (* 10 acc) cur)) digits))

(define-test day8
  (is = 26 (part1 *test-input*))
  (is = 61229 (part2 *test-input*)))
