(ns aoc.2020.day5)

(defn parse-boarding-id [id]
  (apply + (map (fn [[i b]]
                  (* (int (Math/pow 2 i))
                     (case b
                       (\B \R) 1
                       0)))
                (map-indexed vector (reverse id)))))

(defn parse-boarding-ids [input]
  (map parse-boarding-id (.split input "\n")))

(defn part1 [input]
  (apply max (parse-boarding-ids input)))

(defn part2 [input]
  (let [ids (set (parse-boarding-ids input))
        min (apply min ids)
        max (apply max ids)]
    (remove ids (range min (inc max)))))
