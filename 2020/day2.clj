(ns aoc.2020.day2
  (:use clojure.test))

(defn parse-input [^String input]
  (letfn [(parse-line [line]
            (let [[_ min max letter password] (re-matches #"^(\d+)-(\d+) ([a-z]): ([a-z]+)$" line)]
              [(Integer/parseInt min)
               (Integer/parseInt max)
               (get letter 0)
               password]))]
    (map parse-line (.split input "\n"))))

(defn part1 [input]
  (let [entries (parse-input input)]
    (count (filter (fn [[min max letter password]]
                     (<= min (count (filter #(= letter %) password)) max))
                   entries))))

(defn part2 [input]
  (let [entries (parse-input input)]
    (count (filter (fn [[i j letter password]]
                     (let [first  (get password (dec i))
                           second (get password (dec j))]
                       (not (= (= letter first)
                               (= letter second)))))
                   entries))))

(deftest part1-test
  (is (= 2   (part1 (slurp "data/day2-example.txt"))))
  (is (= 638 (part1 (slurp "data/day2-puzzle.txt")))))

(deftest part2-test
  (is (= 1   (part2 (slurp "data/day2-example.txt"))))
  (is (= 699 (part2 (slurp "data/day2-puzzle.txt")))))
