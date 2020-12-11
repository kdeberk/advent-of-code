(ns aoc.2020.day11)

(def directions [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]])

(defn parse-seats [input]
  (loop [lines (map-indexed vector (.split input "\n"))
         seats {}]
    (if (empty? lines)
      (into {} (filter (fn [[_ cell]] (= :empty cell))
                       seats))
      (recur (rest lines)
             (apply merge seats (let [[x line] (first lines)]
                                  (map (fn [[y c]]
                                         {[x y] (case c
                                                  \L :empty
                                                  \. :floor)})
                                       (map-indexed vector line))))))))

(defn get-direct-neighbors [seats]
  (into {}
        (map (fn [k]
               [k (filter seats (map #(mapv + k %) directions))])
             (keys seats))))

(defn get-visible-neighbors [seats max]
  (letfn [(first-neighbor [c d]
            (loop [[x y] (map + c d)]
              (when (and (<= 0 x max) (<= 0 y max))
                (if (seats [x y])
                 [x y]
                 (recur (map + [x y] d))))))]
    (into {}
          (map (fn [k]
                 [k (filter seats
                            (map #(first-neighbor k %) directions))])
               (keys seats)))))

(defn count-occupied [seats]
  (count (filter #(= :occupied %)
                 (vals seats))))

(defn game-of-seats [seats neighbors tolerance]
  (letfn [(count-neighbors [seats c]
            (count (filter #(= :occupied (seats %))
                           (neighbors c))))
          (game-step [seats]
            (into {}
                  (for [[k v] seats]
                    [k (let [c (count-neighbors seats k)]
                         (cond (= 0 c)                :occupied
                               (and (= v :occupied)
                                    (<= tolerance c)) :empty
                               :else                  v))])))]
    (loop [current seats]
      (let [next (game-step current)]
        (if (= next current)
          current
          (recur next))))))

(defn part1 [input]
  (let [seats (parse-seats input)
        neighbors (get-direct-neighbors seats)]
    (count-occupied (game-of-seats seats neighbors 4))))

(defn part2 [input]
  (let [seats (parse-seats input)
        max (count (.split input "\n"))
        neighbors (get-visible-neighbors seats max)]
    (count-occupied (game-of-seats seats neighbors 5))))
