(ns aoc.2020.day1
  (:require [aoc.2020.utils :refer [combinations]]))

(defn calc-expense-report [input size]
  (map (fn [t] (apply * t))
       (filter (fn [t] (= 2020 (apply + t)))
               (combinations size input))))

(defn part1 [input]
  (calc-expense-report input 2))

(defn part2 [input]
  (calc-expense-report input 3))
