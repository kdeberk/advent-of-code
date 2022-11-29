(ns aoc.2020.day1
  (:use clojure.test)
  (:require [aoc.2020.utils :refer [combinations]]))

(defn parse-input [^String input]
  (map #(Integer/parseInt %) (.split input "\n")))

(defn part1 [input]
  (let [input (set (parse-input input))]
    (first (for [x input
                 :let  [y (- 2020 x)]
                 :when (input y)]
             (* x y)))))

(defn part2 [input]
  (let [input   (parse-input input)
        numbers (vec (sort input))
        lookup  (set input)]
    (loop [i 0 j 1]
      (let [x (get numbers i) y (get numbers j) z (- 2020 x y)]
        (cond
          (lookup z) (* x y z)
          (<= z 0)   (recur (+ i 1) (+ i 2))
          :else      (recur i (inc j)))))))

(deftest part1-test
  (is (= 514579 (part1 (slurp "data/day1-example.txt"))))
  (is (= 436404 (part1 (slurp "data/day1-puzzle.txt")))))

(deftest part2-test
  (is (= 241861950 (part2 (slurp "data/day1-example.txt"))))
  (is (= 274879808 (part2 (slurp "data/day1-puzzle.txt")))))
