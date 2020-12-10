(ns aoc.2020.day10)

(defn parse-adapters [input]
  (vec (sort (map #(Integer/parseInt %)
                  (.split input "\n" )))))

(defn part1 [input]
  (letfn [(count-diffs [cur as ds]
            (if (empty? as)
              ds
              (let [[a & as] as
                    d (- a cur)]
                (if (ds d)
                  (recur a as (update ds d inc))
                  (recur a as (assoc ds d 1))))))]
    (let [adapters (parse-adapters input)
          adapters (conj adapters (+ 3 (last adapters)))
          diffs (count-diffs 0 adapters {})]
      (* (diffs 1) (diffs 3)))))


(defn part2 [input]
  (letfn [(count-combinations [cur as]
            (if (empty? as)
              {cur 1}
              (let [xs (count-combinations (first as) (rest as))]
                (assoc xs cur (+ (get xs (+ cur 1) 0)
                                 (get xs (+ cur 2) 0)
                                 (get xs (+ cur 3) 0))))))]
    (let [adapters (parse-adapters input)
          counts (count-combinations 0 adapters)]
      (counts 0))))
