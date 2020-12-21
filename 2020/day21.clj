(ns aoc.2020.day21
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defn parse-input [input]
  (letfn [(parse-food [line]
            (let [[_ ingredients allergens] (re-matches #"([^\(]+) \(contains ([^\)]+)\)" line)]
              [(set (.split ingredients " "))
               (set (.split allergens ", "))]))]
    (into [] (map parse-food (.split input "\n")))))

(defn identify-allergens-1 [foods]
  (letfn [(update-allergens [all-to-ings ings alls]
            (loop [all-to-ings all-to-ings
                   [all & alls] alls]
              (if (not all)
                all-to-ings
                (recur (update all-to-ings all set/intersection ings)
                       alls))))]
    (let [ingredients (set (mapcat first foods))
          allergens   (set (mapcat second foods))]
      (loop [[[ing all] & foods] foods
             all-to-ings (into {} (map (fn [k] [k ingredients]) allergens))]
        (if (not ing)
          all-to-ings
          (recur foods (update-allergens all-to-ings ing all)))))))

(defn identify-allergens-2 [all-to-ings]
  (loop [known      {}
         all-to-ings all-to-ings]
    (let [[all [ing & _]] (first (filter (fn [[all ings]] (= 1 (count ings))) all-to-ings))]
      (if (not all)
        known
        (recur (assoc known all ing)
               (map (fn [[all ings]] [all (remove #{ing} ings)]) all-to-ings))))))

(defn part1 [input]
  (let [foods        (parse-input input)
        all-to-ings  (identify-allergens-1 foods)
        sketchy-ings (set (mapcat second all-to-ings))]
    (count (remove sketchy-ings (mapcat first foods)))))

(defn part2 [input]
  (let [foods       (parse-input input)
        all-to-ings (identify-allergens-1 foods)
        all-to-ing  (identify-allergens-2 all-to-ings)]
    (str/join "," (map all-to-ing (sort (keys all-to-ings))))))
