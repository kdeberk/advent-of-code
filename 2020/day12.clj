(ns aoc.2020.day12)

(defn parse-actions [input]
  (map (fn [action]
         [(case (first action)
            \F :forward
            \R :right
            \L :left
            \N :north
            \W :west
            \E :east
            \S :south)
          (Integer/parseInt (subs action 1))])
       (.split input "\n")))

(defn manhattan-distance [[x y]]
  (let [x (if (< 0 x) x (- x))
        y (if (< 0 y) y (- y))]
    (+ x y)))

(defn part1 [input]
  (letfn [(navigate [[x y d] [action n]]
            (case action
              :forward (case d
                         0   [(+ x n) y d]
                         90  [x (+ y n) d]
                         180 [(- x n) y d]
                         270 [x (- y n) d])
              :east  [(+ x n) y d]
              :north [x (+ y n) d]
              :west  [(- x n) y d]
              :south [x (- y n) d]
              :left  [x y (mod (+ d n) 360)]
              :right [x y (mod (- d n) 360)]))])
  (let [[x y _] (reduce navigate [0 0 0] (parse-actions input))]
    (manhattan-distance [x y])))


(defn part2 [input]
  (letfn [(rotate [[x y] d]
            (case (mod d 360)
              0   [x y]
              90  [(- y) x]
              180 [(- x) (- y)]
              270 [y (- x)]))
          (navigate [[sx sy wx wy] [action n]]
            (case action
              :forward [(+ sx (* n wx)) (+ sy (* n wy)) wx wy]
              :east    [sx sy (+ wx n) wy]
              :north   [sx sy wx (+ wy n)]
              :west    [sx sy (- wx n) wy]
              :south   [sx sy wx (- wy n)]
              :left    (let [[wx' wy'] (rotate [wx wy] n)]
                         [sx sy wx' wy'])
              :right   (let [[wx' wy'] (rotate [wx wy] (- n))]
                         [sx sy wx' wy'])))]
    (let [[x y _ _] (reduce navigate [0 0 10 1] (parse-actions input))]
      (manhattan-distance [x y]))))
