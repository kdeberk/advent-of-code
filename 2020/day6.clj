(ns aoc.2020.day6
  (:require [clojure.set :refer [union intersection]]))

(defn parse-input [input]
  (map (fn [group]
         (map set (.split group "\n")))
       (.split input "\n\n")))

(defn count-questions [input fn]
  (apply + (map #(count (apply fn %)) (parse-input input))))

(defn part1 [input]
  (count-questions input union))

(defn part2 [input]
  (count-questions input intersection))
