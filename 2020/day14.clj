(ns aoc.2020.day14
  (:require [clojure.string :as str]))

(defn parse-lines [input]
  (map (fn [line]
         (let [[dst v] (.split line " = ")]
           (if (= dst "mask")
             [:mask (map-indexed vector (reverse v))]
             (let [[_ m] (re-matches #"mem\[(\d+)\]" dst)]
               [(Integer/parseInt m)
                (Integer/parseInt v)]))))
       (.split input "\n")))

(defn part1 [input]
  (letfn [(apply-mask [val mask]
            (reduce (fn [v [i m]]
                      (case m
                        \0 (bit-clear v i)
                        \1 (bit-set   v i)
                        \X v))
                    val
                    mask))]
    (let [instructions (parse-lines input)
          [final _] (reduce (fn [[mem mask] [dst val]]
                              (if (= dst :mask)
                                [mem val]
                                [(assoc mem dst (apply-mask val mask)) mask]))
                            [{} nil]
                            instructions)]
      (apply + (vals final)))))

(defn part2 [input]
  (letfn [(explode-masks [addr mask]
            (reduce (fn [addrs [i m]]
                      (case m
                        \0 addrs
                        \1 (map #(bit-set % i) addrs)
                        \X (concat (map #(bit-set % i) addrs)
                                   (map #(bit-clear % i) addrs))))
                    [addr]
                    mask))]
    (let [instructions (parse-lines input)
          [final _] (reduce (fn [[mem mask] [dst val]]
                              (if (= dst :mask)
                                [mem val]
                                [(reduce (fn [mem addr]
                                           (assoc mem addr val))
                                         mem
                                         (explode-masks dst mask))
                                 mask]))
                            [{} nil]
                            instructions)]
      (apply + (vals final)))))
