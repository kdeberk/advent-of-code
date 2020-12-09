(ns aoc.2020.day9)

(defn combinations [m coll] 
  (letfn [(comb [m coll]
            (if (= 1 m)
              (map list coll)
              (for [[i x] (map-indexed vector coll)
                    xs    (comb (dec m) (subvec coll (inc i)))]
                (cons x xs))))]
    (comb m coll)))

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
    (letfn [(find-starting-at [i len]
              (let [xs (subvec numbers i (+ i len))
                    sum (apply + xs)]
                (cond
                  (< to-find sum) nil
                  (= to-find sum) (+ (apply min xs) (apply max xs))
                  :else (recur i (inc len)))))
            (find-from [i]
              (or (find-starting-at i 2)
                  (recur (inc i))))]
      (find-from 0))))
