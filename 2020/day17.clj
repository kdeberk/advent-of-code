(ns aoc.2020.day17)

(def directions-3
  (remove #(= % [0 0 0])
          (for [x [-1 0 1] y [-1 0 1] z [-1 0 1]]
            [x y z])))

(def directions-4
  (remove #(= % [0 0 0 0])
          (for [x [-1 0 1] y [-1 0 1] z [-1 0 1] w [-1 0 1]]
            [x y z w])))

(defn parse-input [input]
  (map first (filter (fn [[coords status]]
                       (= :active status))
                     (mapcat (fn [[x row]]
                               (map-indexed (fn [y cell]
                                              [[x y] (case cell
                                                       \. :inactive
                                                       \# :active)])
                                            row))
                             (map-indexed vector (.split input "\n"))))))

(defn count-active-neighbors [coords cells directions]
  (count (filter cells (map #(mapv + coords %) directions))))

;; TODO: somehow merge the next 2
(defn game-of-life-step-3 [prev-cells]
  (let [[min-x min-y min-z] (reduce #(mapv min %1 %2) prev-cells)
        [max-x max-y max-z] (reduce #(mapv max %1 %2) prev-cells)]
    (set (filter
          identity
          (for [x (range (dec min-x) (+ max-x 2))
                y (range (dec min-y) (+ max-y 2))
                z (range (dec min-z) (+ max-z 2))]
            (let [coords [x y z]
                  count  (count-active-neighbors coords prev-cells directions-3)
                  active (contains? prev-cells coords)]
              (cond (and active (or (< count 2) (< 3 count)))
                    nil
                    (and (not active) (= count 3))
                    coords
                    active
                    coords)))))))

(defn game-of-life-step-4 [prev-cells]
  (let [[min-x min-y min-z min-w] (reduce #(mapv min %1 %2) prev-cells)
        [max-x max-y max-z max-w] (reduce #(mapv max %1 %2) prev-cells)]
    (set (filter
          identity
          (for [x (range (dec min-x) (+ max-x 2))
                y (range (dec min-y) (+ max-y 2))
                z (range (dec min-z) (+ max-z 2))
                w (range (dec min-w) (+ max-w 2))]
            (let [coords [x y z w]
                  count  (count-active-neighbors coords prev-cells directions-4)
                  active (contains? prev-cells coords)]
              (cond (and active (or (< count 2) (< 3 count)))
                    nil
                    (and (not active) (= count 3))
                    coords
                    active
                    coords)))))))

(defn part1 [input]
  (let [active (parse-input input)
        active (set (map #(concat % [0]) active))]
    (loop [turn 0 active active]
      (if (= turn 6)
        (count active)
        (recur (inc turn) (game-of-life-step-3 active))))))

(defn part2 [input]
  (let [active (parse-input input)
        active (set (map #(concat % [0 0]) active))]
    (loop [turn 0 active active]
      (if (= turn 6)
        (count active)
        (recur (inc turn) (game-of-life-step-4 active))))))
