(ns aoc.2020.day19
  (:require [clojure.string :as str]))

(defn parse-grammar [lines]
  (letfn [(parse-element [el]
            (if (= \" (get el 0))
              (subs el 1 (dec (count el)))
              (Integer/parseInt el)))
          (parse-rule [rule]
            (map parse-element (.split rule " ")))
          (parse-rules [rules]
            (map parse-rule (.split rules " [|] ")))
          (parse-line [line]
            (let [[idx rules] (.split line ": ")]
              [(Integer/parseInt idx) (parse-rules rules)]))]
    (into {} (map parse-line (.split lines "\n")))))

(defn parse-input [input]
  (let [[grammar messages] (.split input "\n\n")]
    [(parse-grammar grammar)
     (.split messages "\n")]))

(defn validate-text [text grammar]
  (letfn [(read-rule [text part]
            (loop [texts [text] part part]
              (cond
                (empty? texts) nil
                (empty? part)  texts
                :else (let [[x & xs] part]
                        (cond
                          (int? x)    (recur (mapcat #(read-rules % x) texts)
                                             xs)
                          (string? x) (recur (map #(subs % (count x))
                                                  (filter #(str/starts-with? % x) texts))
                                             xs))))))
          (read-rules [text rule-idx]
            (mapcat #(read-rule text %) (grammar rule-idx)))]
    (some empty? (read-rules text 0))))

(defn part1 [input]
  (let [[grammar messages] (parse-input input)]
    (count (filter #(validate-text % grammar) messages))))

(defn part2 [input]
  (let [[grammar messages] (parse-input input)
        grammar (assoc grammar
                       8  '((42) (42 8))
                       11 '((42 31) (42 11 31)))]
    (count (filter #(validate-text % grammar) messages))))


