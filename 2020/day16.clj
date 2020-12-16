(ns aoc.2020.day16
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (letfn [(parse-ticket [line]
            (map #(Integer/parseInt %) (.split line ",")))
          (parse-rules [lines]
            (into {} (map (fn [line]
                            (let [[_ name mina maxa minb maxb] (re-matches #"([^:]+): (\d+)-(\d+) or (\d+)-(\d+)" line)]
                              [name (map #(Integer/parseInt %) [mina maxa minb maxb])]))
                          (.split lines "\n"))))])
  (let [[rules your-ticket nearby-tickets] (.split input "\n\n")
        your-ticket    (second (.split your-ticket "\n"))
        nearby-tickets (drop 1 (.split nearby-tickets "\n"))]
    [(parse-rules rules)
     (vec (parse-ticket your-ticket))
     (map parse-ticket nearby-tickets)]))

(defn matching-rules [val rules]
  (into {} (filter (fn [[_ [a b c d]]]
                     (or (<= a val b) (<= c val d)))
                   rules)))

(defn part1 [input]
  (let [[rules _ nearby-tickets] (parse-input input)]
    (reduce + (mapcat (fn [ticket]
                        (filter #(empty? (matching-rules % rules))
                                ticket))
                      nearby-tickets))))

(defn part2 [input]
  (let [[rules your-ticket nearby-tickets] (parse-input input)
        valid-tickets (filter (fn [ticket]
                                (every? #(not (empty? (matching-rules % rules)))
                                        ticket))
                              nearby-tickets)
        ;; Create a map of ticket idx to the rules that match all values at those idxs
        idx-to-rules  (into {} (map (fn [[idx _]] [idx rules])
                                    (map-indexed vector your-ticket)))
        idx-to-rules  (reduce (fn [rules ticket]
                                (into {} (map (fn [[idx val]]
                                                [idx (matching-rules val (rules idx))])
                                              (map-indexed vector ticket))))
                              idx-to-rules
                              valid-tickets)
        ;; By repeatedly identifying a idx-rule pair with a single rule, and removing
        ;; that rule from the other idx-rules pairs, we manage to fix each field to an idx.
        field-to-idx (loop [idx-to-rules idx-to-rules
                            field-to-idx {}]
                       (let [[idx rules]    (first (filter (fn [[_ v]] (= 1 (count v))) idx-to-rules))
                             [field-name _] (first rules)]
                         (if idx
                           (recur (into {} (map (fn [[idx rules]]
                                                  [idx (dissoc rules field-name)])
                                                (dissoc idx-to-rules idx)))
                                  (assoc field-to-idx field-name idx))
                           field-to-idx)))]
    (reduce * (map (fn [[_ idx]]
                     (your-ticket idx))
                   (filter (fn [[name idx]]
                             (str/starts-with? name "departure"))
                           field-to-idx)))))
