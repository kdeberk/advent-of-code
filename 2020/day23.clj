(ns aoc.2020.day23
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (map #(- (int %) (int \0)) input))

(defn complete-circle [cups total]
  (loop [[c & cs] (concat cups (range (inc (apply max cups)) (inc total)))
         m        {total (first cups)
                   (last cups) (first cups)}]
    (if (empty? cs)
      m
      (recur cs (assoc m c (first cs))))))

(defn play-round [cups current]
  (let [old-next    (cups current)
        middle      (cups old-next)
        end-slice   (cups middle)
        new-next    (cups end-slice)
        dst         (loop [dst (dec current)]
                      (cond
                        (= 0 dst) (recur (count cups))
                        (#{old-next middle end-slice} dst) (recur (dec dst))
                        :else dst))
        after-dst   (cups dst)]
    (assoc cups
           current   new-next
           dst       old-next
           end-slice after-dst)))

(defn play-game [cups start n-rounds]
  (loop [cups    cups
         current start
         n       0]
    (if (= n n-rounds)
      cups
      (let [cups (play-round cups current)]
        (recur cups (cups current) (inc n))))))


(defn part1 [input]
  (let [cups      (parse-input input)
        start     (first cups)
        cups      (complete-circle cups (count cups))
        final     (play-game cups start 100)]
    (loop [current 1 chain []]
      (let [next (final current)]
        (if (= 1 next)
          (println (str/join (map #(char (+ (int \0) %)) chain)))
          (recur next (conj chain next)))))))

;; Slow
(defn part2 [input]
  (let [cups  (parse-input input)
        start (first cups)
        cups  (complete-circle cups 1000000)
        final (play-game cups start 10000000)]
    (let [a (final 1) b (final a)]
      (* a b))))
