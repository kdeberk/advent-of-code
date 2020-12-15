(ns aoc.2020.day15)

(defn parse-starting-numbers [input]
  (map #(Long/parseLong %) (.split input ",")))

(defn memory-game [starting nth]
  (let [mem (java.util.HashMap. (into {} (map-indexed (fn [x y] [y (inc x)]) starting)))]
    (loop [turn (inc (count starting))
           last (last starting)]
      (let [next (if-let [idx (.get mem last)]
                   (- (dec turn) idx)
                   0)]
        (if (= nth turn)
          next
          (do
            (.put mem last (dec turn))
            (recur (inc turn) next)))))))

(defn part1 [input]
  (memory-game (parse-starting-numbers input) 2020))

(defn part2 [input]
  (memory-game (parse-starting-numbers input) 30000000))
