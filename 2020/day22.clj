(ns aoc.2020.day22)

(defn parse-input [input]
  (map (fn [lines]
         (let [[_ & cards] (.split lines "\n")]
           (into [] (map #(Integer/parseInt %) cards))))
       (.split input "\n\n")))

(defn play-game [cards-a cards-b]
  (loop [as cards-a
         bs cards-b]
    (cond
      (empty? as) [2 bs]
      (empty? bs) [1 as]
      :else (let [[a & as] as
                  [b & bs] bs]
              (if (< a b)
                (recur as (conj (vec bs) b a))
                (recur (conj (vec as) a b) bs))))))

(defn play-recursive-game [cards-a cards-b]
  (loop [as   cards-a
         bs   cards-b
         seen #{}]
    (cond
      (seen [as bs]) [1 as]
      (empty? as)    [2 bs]
      (empty? bs)    [1 as]
      :else (let [seen (conj seen [as bs])
                  [a & as] as
                  [b & bs] bs
                  [winner _] (cond
                               (and (<= a (count as)) (<= b (count bs)))
                               (play-recursive-game (subvec (vec as) 0 a) (subvec (vec bs) 0 b))
                               (< a b) [2 nil]
                               :else   [1 nil])]
              (if (= 1 winner)
                (recur (conj (vec as) a b) bs seen)
                (recur as (conj (vec bs) b a) seen))))))

(defn calculate-score [cards]
  (reduce + (map-indexed (fn [i c] (* (inc i) c))
                         (reverse cards))))

(defn part1 [input]
  (let [[cards-a cards-b] (parse-input input)
        [_ deck] (play-game cards-a cards-b)]
    (calculate-score deck)))

(defn part2 [input]
  (let [[cards-a cards-b] (parse-input input)
        [_ deck] (play-recursive-game cards-a cards-b)]
    (calculate-score deck)))
