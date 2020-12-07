(ns aoc.2020.day7
  (:require [clojure.string :as str]
            [clojure.set :refer [union]]))

(defn parse-rule [line]
  (let [[_ parent children] (re-matches #"(\w+ \w+) bags contain (.*+)" line)]
    (if (str/includes? line "no other bags")
      {parent #{}}
      (let [cs (map (fn [c]
                      (let [[_ count color] (re-matches #"(\d+) (\w+ \w+).*" c)]
                        [(Integer/parseInt count) color]))
                    (.split children ", "))]
        {parent cs}))))

(defn parse-rules [input]
  (apply merge-with union
         (map parse-rule
              (.split input "\n"))))

(defn all-parents [b bags]
  (letfn [(search [c found]
            (if-let [ps (bags c)]
              (map #(search % (union found #{%})) ps)
              found))]
    (apply union (flatten (search b #{})))))

(defn part1 [input]
  (let [rules (parse-rules input)
        child-to-parents (apply merge-with union
                                (flatten
                                 (map (fn [[p cs]]
                                        (map (fn [[_ c]] {c #{p}}) cs))
                                      rules)))]
    (count (all-parents "shiny gold" child-to-parents))))

(defn count-bag+children [bag rules]
  (let [cs (rules bag)]
    (if (< 0 (count cs))
      (apply + 1 (map (fn [[n c]]
                        (* n (count-children c rules)))
                      cs))
      1)))

(defn part2 [input]
  (let [rules (parse-rules input)]
    (dec (count-bag+children "shiny gold" rules))))
