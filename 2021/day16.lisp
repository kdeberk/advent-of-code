(in-package :day16)

(defvar *input* (utils:read-input "day16.txt"))

(defstruct bit-stream bits (index 0))

(defmethod read-number ((s bit-stream) n)
  (let ((bits (bit-stream-bits s)))
    (setf (bit-stream-bits s) (subseq bits n))
    (incf (bit-stream-index s) n)
    (parse-integer (subseq bits 0 n) :radix 2)))

(defstruct packet version type-id literal sub-packets)

(defun parse-transmission (transmission)
  (labels ((hex-to-bit-stream (hex)
             (let ((bits (format nil "~B" (parse-integer hex :radix 16))))
               (let ((prefix-length (- (* 4 (length hex)) (length bits))))
                 (make-bit-stream :bits (concatenate 'string (format nil "~v@{~a~:*~}" prefix-length "0") bits)))))
           (read-packet (stream)
             (let ((version (read-number stream 3))
                   (type-id (read-number stream 3)))
               (cond ((= 4 type-id)
                      (let ((value 0))
                        (loop
                          (let ((header (read-number stream 1))
                                (block  (read-number stream 4)))
                            (setf value (+ (* value 16) block))
                            (when (= 0 header)
                              (return (make-packet :version version :type-id type-id :literal value)))))))
                     (t (if (= 0 (read-number stream 1))
                            (let ((length (read-number stream 15))
                                  (start (bit-stream-index stream)))
                              (make-packet :version version :type-id type-id
                                           :sub-packets (loop until (<= (+ start length) (bit-stream-index stream))
                                                              collect (read-packet stream))))
                            (make-packet :version version :type-id type-id
                                         :sub-packets (loop repeat (read-number stream 11)
                                                            collect (read-packet stream)))))))))
    (read-packet (hex-to-bit-stream transmission))))

(defun part1 (input)
  (labels ((count-versions (packet)
             (+ (packet-version packet)
                (apply #'+ (mapcar #'count-versions (packet-sub-packets packet))))))
    (count-versions (parse-transmission input))))

(defun part2 (input)
  (labels ((eval-packet (packet)
             (with-slots (type-id sub-packets literal) packet
               (let ((sub-evals (mapcar #'eval-packet sub-packets)))
                 (case type-id
                   (0 (apply #'+ sub-evals))
                   (1 (apply #'* sub-evals))
                   (2 (apply #'min sub-evals))
                   (3 (apply #'max sub-evals))
                   (4 literal)
                   (5 (if (apply #'> sub-evals) 1 0))
                   (6 (if (apply #'< sub-evals) 1 0))
                   (7 (if (apply #'= sub-evals) 1 0)))))))
    (eval-packet (parse-transmission input))))


(define-test day16
  (is = 16 (part1 "8A004A801A8002F478"))
  (is = 12 (part1 "620080001611562C8802118E34"))
  (is = 23 (part1 "C0015000016115A2E0802F182340"))
  (is = 31 (part1 "A0016C880162017C3686B18A3D4780"))
  (is = 3 (part2 "C200B40A82"))
  (is = 54 (part2 "04005AC33890"))
  (is = 7 (part2 "880086C3E88112"))
  (is = 9 (part2 "CE00C43D881120"))
  (is = 1 (part2 "D8005AC2A8F0"))
  (is = 0 (part2 "F600BC2D8F"))
  (is = 0 (part2 "9C005AC2F8F0"))
  (is = 1 (part2 "9C0141080250320F1802104A08")))
