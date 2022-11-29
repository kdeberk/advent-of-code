(ns aoc.2020.day3
  (:use clojure.test))

(defn count-trees [lines dy dx]
  (loop [y 0 r 0 c 0]
    (if (<= (count lines) y)
      c
      (let [line (get lines y)
            x    (mod (* r dx) (count line))
            cell (get line x)]
        (if (= \# cell)
          (recur (+ y dy) (inc r) (inc c))
          (recur (+ y dy) (inc r) c))))))

(defn part1 [^String input]
  (count-trees (.split input "\n") 1 3))

(defn part2 [^String input]
  (let [lines  (.split input "\n")
        slopes [[1 1] [1 3] [1 5] [1 7] [2 1]]]
    (apply * (map (fn [[dy dx]]
                    (count-trees lines dy dx))
                  slopes))))

(deftest part1-test
  (is (= 7   (part1 (slurp "data/day3-example.txt"))))
  (is (= 294 (part1 (slurp "data/day3-puzzle.txt")))))

(deftest part2-test
  (is (= 336        (part2 (slurp "data/day3-example.txt"))))
  (is (= 5774564250 (part2 (slurp "data/day3-puzzle.txt")))))
