(ns aoc.2020.day14
  (:require [clojure.string :as str]))

(defn parse-lines [input]
  (map (fn [line]
         (let [[dst v] (.split line " = ")]
           (if (= dst "mask")
             [:mask v]
             (let [[_ m] (re-matches #"mem\[(\d+)\]" dst)]
               [(Integer/parseInt m)
                (str/join "" (let [bits (Integer/toBinaryString (Integer/parseInt v))]
                               (concat (map (fn [_] \0) (range 0 (- 36 (count bits))))
                                       bits)))]))))
       (.split input "\n")))

(defn apply-mask [val mask]
  (str/join "" (map (fn [v m]
                      (case m \0 0 \1 1 \X v))
                    val
                    mask)))

(defn part1 [input]
  (loop [instructions (parse-lines input)
         mem {}
         mask nil]
    (if (empty? instructions)
      (apply + (map #(Long/parseLong % 2) (vals mem)))
      (let [[dst v] (first instructions)]
        (if (= dst :mask)
          (recur (rest instructions) mem v)
          (recur (rest instructions)
                 (assoc mem dst (apply-mask v mask))
                 mask))))))

(defn explode-mask [val mask]
  (loop [acc [val] mask mask]
    (if (empty? mask)
      acc
      (case (first mask)
        \0 (recur acc (rest mask))
        \1 (recur (map #(bit-set % (dec (count mask))) acc)
                  (rest mask))
        \X (recur (concat (map #(bit-clear % (dec (count mask))) acc)
                          (map #(bit-set % (dec (count mask))) acc))
                  (rest mask))))))

(defn part2 [input]
  (loop [instructions (parse-lines input)
         mem {}
         mask nil]
    (if (empty? instructions)
      (apply + (map #(Long/parseLong % 2) (vals mem)))
      (let [[dst v] (first instructions)]
        (if (= dst :mask)
          (recur (rest instructions) mem v)
          (recur (rest instructions)
                 (loop [mem mem dsts (explode-mask dst mask)]
                   (if (empty? dsts)
                     mem
                     (recur (assoc mem (first dsts) v) (rest dsts))))
                 mask))))))
