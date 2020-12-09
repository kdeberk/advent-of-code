(ns aoc.2020.day9
  (:require [aoc.2020.utils :refer [combinations]]))

(defn parse-input [input]
  (vec (map #(Long/parseLong %) (.split input "\n"))))

(defn part1 [input len-preamble]
  (let [numbers (parse-input input)]
    (letfn [(find [i]
              (let [sums (set (map #(apply + %)
                                   (combinations 2 (subvec numbers (- i len-preamble) i))))]
                (if (sums (numbers i))
                  (recur (inc i))
                  (numbers i))))]
      (find len-preamble))))

(defn part2 [input len-preamble]
  (let [numbers (parse-input input)
        to-find (part1 input len-preamble)]
    (letfn [(find-fixed-beg [beg end sum]
              (cond
                (< to-find sum) nil
                (= to-find sum) (let [xs (subvec numbers beg end)]
                                  (+ (apply min xs) (apply max xs)))
                :else (recur beg (inc end) (+ sum (numbers end)))))
            (find [beg]
              (or (let [end (+ beg 2)]
                    (find-fixed-beg beg end (apply + (subvec numbers beg end))))
                  (recur (inc beg))))]
      (find 0))))
