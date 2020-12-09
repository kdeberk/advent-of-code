(ns aoc.2020.utils)

(defn combinations [m coll] 
  (letfn [(comb [m coll]
            (if (= 1 m)
              (map list coll)
              (for [[i x] (map-indexed vector coll)
                    xs    (comb (dec m) (subvec coll (inc i)))]
                (cons x xs))))]
    (comb m coll)))
