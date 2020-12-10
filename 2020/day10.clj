(ns aoc.2020.day10)

(defn parse-adapters [input]
  (vec (sort (map #(Integer/parseInt %)
                  (.split input "\n" )))))

(defn part1 [input]
  (let [numbers (parse-adapters input)
        [_ counts] (reduce (fn [[prev diffs] cur]
                             (let [d (- cur prev)]
                               [cur (assoc diffs d (inc (get diffs d 0)))]))
                           [0 {}]
                           (conj numbers (+ (last numbers) 3)))]
    (* (counts 1) (counts 3))))


(defn part2 [input]
  (let [adapters (parse-adapters input)
        counts (reduce (fn [cs cur]
                         (assoc cs cur (+ (get cs (+ cur 1) 0)
                                          (get cs (+ cur 2) 0)
                                          (get cs (+ cur 3) 0))))
                       {(+ 3 (last adapters)) 1}
                       (reverse (cons 0 adapters)))]
    (counts 0)))
