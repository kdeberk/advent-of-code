(in-package :day22)

(defvar *test-input-1* (utils:read-input "day22_test1.txt"))
(setf *test-input-2* (utils:read-input "day22_test2.txt"))
(setf *test-input-3* (utils:read-input "day22_test3.txt"))
(setf *input* (utils:read-input "day22.txt"))

(defstruct action to start-x end-x start-y end-y start-z end-z)

(defun parse-input (input)
  (mapcar (lambda (line)
            (cl-ppcre:register-groups-bind (to start-x end-x start-y end-y start-z end-z)
                ("(on|off) x=([-0-9]+)..([-0-9]+),y=([-0-9]+)..([-0-9]+),z=([-0-9]+)..([-0-9]+)" line)
              (let ((start-x (when start-x (parse-integer start-x))) (end-x (when end-x (parse-integer end-x)))
                    (start-y (when start-y (parse-integer start-y))) (end-y (when end-y (parse-integer end-y)))
                    (start-z (when start-z (parse-integer start-z))) (end-z (when end-z (parse-integer end-z))))
                (make-action :to (equal to "on")
                             :start-x start-x
                             :end-x end-x
                             :start-y start-y
                             :end-y end-y
                             :start-z start-z
                             :end-z end-z))))
          (str:split #\Newline input)))

(defstruct range values start-idx end-idx)
(defstruct ranges x y z)

(defun range-start (range)
  (aref (range-values range) (range-start-idx range)))

(defun range-end (range)
  (aref (range-values range) (range-end-idx range)))

(defun read-ranges (actions)
  (labels ((combine (start-fn end-fn)
             (let ((xs (sort (remove-duplicates (append (mapcar start-fn actions)
                                                        ;; (mapcar end-fn actions)
                                                        (mapcar (lambda (x) (1+ (funcall end-fn x))) actions)))
                             #'<)))
               (make-range :values (make-array (1+ (length xs)) :initial-contents (append xs (list (1+ (first (last xs))))))
                           :start-idx 0 :end-idx (length xs)))))
    (make-ranges :x (combine #'action-start-x #'action-end-x)
                 :y (combine #'action-start-y #'action-end-y)
                 :z (combine #'action-start-z #'action-end-z))))

(defun split-ranges (ranges)
  (labels ((split-range (range)
             (with-slots (values start-idx end-idx) range
               (if (= 1 (- end-idx start-idx))
                   (list range)
                   (let ((pivot (+ start-idx (floor (/ (- end-idx start-idx) 2)))))
                     (list (make-range :values values :start-idx start-idx :end-idx pivot)
                           (make-range :values values :start-idx pivot :end-idx end-idx)))))))
    (loop for x in (split-range (ranges-x ranges))
          append (loop for y in (split-range (ranges-y ranges))
                       append (loop for z in (split-range (ranges-z ranges))
                                    collect (make-ranges :x x :y y :z z))))))

(defun overlapping-volume (action ranges)
  (labels ((overlap (range a-start a-end)
             (let ((start (max a-start (range-start range)))
                   (end (min a-end (1- (range-end range)))))
               (if (<= start end)
                   (1+ (- end start))
                   0))))
    (* (overlap (ranges-x ranges) (action-start-x action) (action-end-x action))
       (overlap (ranges-y ranges) (action-start-y action) (action-end-y action))
       (overlap (ranges-z ranges) (action-start-z action) (action-end-z action)))))

(defun count-on-nodes (ranges actions)
  (let ((overlapping (remove-if-not (lambda (action)
                                      (< 0 (overlapping-volume action ranges)))
                                    actions)))
    (cond ((not overlapping) 0)
          ((= 1 (length overlapping))
           (if (action-to (first overlapping))
               (overlapping-volume (first overlapping) ranges)
               0))
          (t (let ((subs (split-ranges ranges)))
               (cond ((< 1 (length subs))
                      (loop for sub in subs
                            sum (count-on-nodes sub overlapping)))
                     ((action-to (car (last overlapping)))
                      (overlapping-volume (car (last overlapping)) ranges))
                     (t 0)))))))

(defun part1 (input)
  (let ((actions (remove nil (mapcar (lambda (action)
                                       (with-slots (start-x start-y start-z end-x end-y end-z) action
                                         (unless (or (< end-x -50) (< end-y -50) (< end-z -50)
                                                     (< 50 start-x) (< 50 start-y) (< 50 start-z))
                                           (setf (action-start-x action) (max start-x -50)
                                                 (action-start-y action) (max start-y -50)
                                                 (action-start-z action) (max start-z -50)
                                                 (action-end-x action) (min end-x 50)
                                                 (action-end-y action) (min end-y 50)
                                                 (action-end-z action) (min end-z 50))
                                           action)))
                                     (parse-input input)))))
    (let ((ranges (read-ranges actions)))
      (count-on-nodes ranges actions))))

(defun part2 (input)
  (let* ((actions (parse-input input))
         (ranges (read-ranges actions)))
    (count-on-nodes ranges actions)))

