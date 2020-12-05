(ns aoc.2020.day5)

(defn parse-boarding-ids [input]
  (map (fn [line]
         (apply + (map (fn [[i b]]
                         (* (int (Math/pow 2 (- 9 i)))
                            (case b
                              (\B \R) 1
                              0)))
                       (map-indexed vector line))))
       (.split input "\n")))

(defn part1 [input]
  (apply max (parse-boarding-ids input)))

(defn part2 [input]
  (let [ids (set (parse-boarding-ids input))
        min (apply min ids)
        max (apply max ids)]
    (filter (fn [id]
              (not (contains? ids id)))
            (range min (inc max)))))
