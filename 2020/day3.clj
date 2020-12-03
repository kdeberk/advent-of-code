(ns aoc.2020.day3)

(defn count-trees [lines dx dy]
  (let [;; Get the lines for the slope
        rows (map second
                  (filter (fn [[y _]]
                            (= 0 (mod y dy)))
                          (map-indexed vector lines)))
        ;; Get the cells for each line
        cells (map (fn [[y line]]
                     (get line (mod (* y dx) (.length line))))
                   (map-indexed vector rows))]
    (count (filter #(= \# %) cells))))

(defn part1 [input]
  (count-trees (.split input "\n") 3 1))

(defn part2 [input]
  (let [lines  (.split input "\n")
        slopes [[1 1] [3 1] [5 1] [7 1] [1 2]]]
    (apply * (map (fn [[dx dy]]
                    (count-trees lines dx dy))
                  slopes))))
