(ns aoc.2020.day1)

(defn combinations [m coll] 
  (letfn [(comb [m start]
            (if (= 1 m)
              (for [x (subvec coll start)]
                (list x))
              (for [x (range start (count coll))
                    xs (comb (dec m) (inc x))]
                (cons (get coll x) xs))))]
    (comb m 0)))

(defn calc-expense-report [input size]
  (map (fn [t] (apply * t))
       (filter (fn [t] (= 2020 (apply + t)))
               (combinations size input))))

(defn part1 [input]
  (calc-expense-report input 2))

(defn part2 [input]
  (calc-expense-report input 3))
