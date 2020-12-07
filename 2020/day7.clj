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

(defn find-all-parents [bag child-to-parents]
  (letfn [(search [b found]
            (if-let [parents (child-to-parents b)]
              (apply union (map #(search % (conj found %)) parents))
              found))]
    (search bag #{})))

(defn part1 [input]
  (let [rules (parse-rules input)
        child-to-parents (apply merge-with union
                                (mapcat (fn [[parent children]]
                                          (map (fn [[_ child]] {child #{parent}}) children))
                                        rules))]
    (count (find-all-parents "shiny gold" child-to-parents))))

(defn count-bag+children [bag rules]
  (let [cs (rules bag)]
    (apply + 1 (map (fn [[n c]]
                      (* n (count-bag+children c rules)))
                    cs))))

(defn part2 [input]
  (let [rules (parse-rules input)]
    (dec (count-bag+children "shiny gold" rules))))
