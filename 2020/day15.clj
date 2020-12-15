(ns aoc.2020.day15)

(defn parse-starting-numbers [input]
  (map #(Integer/parseInt %) (.split input ",")))

(defn memory-game [starting nth]
  (loop [turn (inc (count starting))
         last (last starting)
         mem  (into {} (map-indexed (fn [x y] [y (inc x)]) starting))]
    (let [next (if-let [idx (mem last)]
                 (- (dec turn) idx)
                 0)]
      (if (= turn nth)
        next
        (recur (inc turn) next (assoc mem last (dec turn)))))))


(defn part1 [input]
  (memory-game (parse-starting-numbers input) 2020))

(defn part2 [input]
  (memory-game (parse-starting-numbers input) 30000000))
