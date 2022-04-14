(in-package :day23)

;; hallway with 2 x 1 x 1 x 1 x 1 x 2 openings
;; 4 stacks
;; each turn:
;; - shell moves out of room into one of the positions
;; - shell moves into designated room

(defstruct burrow rooms hallway (score 0))

(setf *test-input* (make-burrow :rooms '((#\B #\A) (#\C #\D) (#\B #\C) (#\D #\A))
                                :hallway '(nil nil :skip nil :skip nil :skip nil :skip nil nil)))
(setf *input* (make-burrow :rooms '((#\C #\B) (#\D #\A) (#\D #\B) (#\A #\C))
                             :hallway '(nil nil :skip nil :skip nil :skip nil :skip nil nil)))

;; TODO
(setf *final-rooms* '((#\A #\A #\A #\A) (#\B #\B #\B #\B) (#\C #\C #\C #\C) (#\D #\D #\D #\D)))

(defmethod done? ((b burrow))
  (equal (burrow-rooms b) *final-rooms*))

(defun weight (shell)
  (cond ((char= #\A shell) 1)
        ((char= #\B shell) 10)
        ((char= #\C shell) 100)
        ((char= #\D shell) 1000)))

(defun possible-leaves (burrow from-room)
  "free places returns possible destinations + distances for item in room"
  (let ((start (cond ((char= #\A from-room) 2)
                     ((char= #\B from-room) 4)
                     ((char= #\C from-room) 6)
                     ((char= #\D from-room) 8))))
    (with-slots (hallway) burrow
      (append (loop for idx from start downto 0 and dist from 0
                    while (or (eq :skip (nth idx hallway)) (null (nth idx hallway)))
                    if (null (nth idx hallway))
                      collect (list idx dist) into pos
                    finally (return pos))
              (loop for idx from start below (length hallway) and dist from 0
                    while (or (eq :skip (nth idx hallway)) (null (nth idx hallway)))
                    if (null (nth idx hallway))
                      collect (list idx dist) into pos
                    finally (return pos))))))

(defun possible-enters (burrow into-room)
  (let ((dest (cond ((char= #\A into-room) 2)
                    ((char= #\B into-room) 4)
                    ((char= #\C into-room) 6)
                    ((char= #\D into-room) 8))))
    (with-slots (hallway) burrow
      (remove nil (loop for el in hallway and idx from 0
                        if (equal into-room el)
                          collect (if (< dest idx)
                                      (loop for i from (1- idx) downto dest and dist from 1
                                            always (or (eq :skip (nth i hallway))
                                                       (null (nth i hallway)))
                                            finally (return (list idx dist)))
                                      (loop for i from (1+ idx) to dest and dist from 1
                                            always (or (eq :skip (nth i hallway))
                                                       (null (nth i hallway)))
                                            finally (return (list idx dist)))))))))

(defun next-moves (b max-room-size)
  (with-slots (rooms hallway score) b
    (labels ((move-first-out (room dest idx)
               (mapcar (lambda (pos-and-dist)
                         (make-burrow :rooms (append (subseq rooms 0 idx) (list (cdr room)) (subseq rooms (1+ idx)))
                                      :hallway (let ((hallway (apply #'list hallway)))
                                                 (setf (nth (first pos-and-dist) hallway) (car room))
                                                 hallway)
                                      :score (+ score (* (weight (car room)) (+ (- max-room-size (length room))
                                                                                (second pos-and-dist))))))

                       (possible-leaves b dest)))
             (move-into-room (room dest idx)
               (mapcar (lambda (pos-and-dist)
                         (make-burrow :rooms (append (subseq rooms 0 idx) (list (cons dest room)) (subseq rooms (1+ idx)))
                                      :hallway (let ((hallway (apply #'list hallway)))
                                                 (setf (nth (first pos-and-dist) hallway) nil)
                                                 hallway)
                                      :score (+ score (* (weight dest) (+ (- max-room-size (length room))
                                                                          (second pos-and-dist))))))
                       (possible-enters b dest))))

      (loop for room in rooms and dest in (list #\A #\B #\C #\D) and idx from 0
            append (cond ((remove dest room) ;; room contains something else than intended targt
                          (move-first-out room dest idx))
                         ((member dest hallway)
                          (move-into-room room dest idx)))))))

(defun move-shells (burrow)
  (let ((lowest 100000)
        (hsh (make-hash-table :test 'equal))
        (room-size (length (first (burrow-rooms burrow)))))
    (let ((queue (next-moves burrow room-size)))
      (loop
         (when (null queue)
           (return lowest))
         (let* ((nxt (pop queue))
                (key (list (burrow-rooms nxt) (burrow-hallway nxt))))
           (let ((found (gethash key hsh)))
             (when (or (not found) (< (burrow-score nxt) found))
               (setf (gethash key hsh) (burrow-score nxt))
               (cond ((done? nxt)
                      (setf lowest (min lowest (burrow-score nxt)))
                      (print lowest))
                     ((< (burrow-score nxt) lowest)
                      (setf queue (append queue (next-moves nxt room-size))))))))))))

(defun part1 (input)
  (move-shells input))

(defun part2 (input)
  (with-slots (rooms) input
    (let ((new-rooms (mapcar (lambda (x y) (append (list (car x))  y (cdr x)))
                             rooms
                             '((#\D #\D) (#\C #\B) (#\B #\A) (#\A #\C)))))
      (move-shells (make-burrow :rooms new-rooms
                                :hallway (burrow-hallway input))))))
