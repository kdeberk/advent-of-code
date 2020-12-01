(ns aoc.2020.day1)

(defn combinations [m coll] 
  (letfn [(comb [m coll]
            (if (= 1 m)
              (map list coll)
              (for [[i x] (map-indexed vector coll)
                    xs    (comb (dec m) (subvec coll (inc i)))]
                (cons x xs))))]
    (comb m coll)))

(defn calc-expense-report [input size]
  (map (fn [t] (apply * t))
       (filter (fn [t] (= 2020 (apply + t)))
               (combinations size input))))

(defn part1 [input]
  (calc-expense-report input 2))

(defn part2 [input]
  (calc-expense-report input 3))
