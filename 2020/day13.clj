(ns aoc.2020.day13)

(defn parse-input [input]
  (let [[t bs] (.split input "\n")]
    [(Integer/parseInt t)
     (map (fn [[i b]]
            [(biginteger (Integer/parseInt b))
             (biginteger i)])
          (filter (fn [[_ b]]
                    (re-matches #"[0-9]+" b))
                  (map-indexed vector (.split bs ","))))]))

(defn part1 [input]
  (let [[t bs]   (parse-input input)
        waits    (map (fn [[b _]]
                        [b (- b (mod t b))])
                      bs)
        [b wait] (apply min-key second waits)]
    (int (* b wait))))

(defn extended-euclid [a b]
  (loop [[s s'] [(biginteger 0) (biginteger 1)]
         [r r'] [b a]
         [t t'] [(biginteger 1) (biginteger 0)]]
    (if (not (zero? r))
      (let [q (.divide r' r)]
        (recur [(.subtract s' (.multiply q s)) s]
               [(.subtract r' (.multiply q r)) r]
               [(.subtract t' (.multiply q t)) t]))
      [r' s' t'])))

(defn solve-modulo-equations [xs]
  ;; From: https://homepages.math.uic.edu/~leon/mcs425-s08/handouts/chinese_remainder.pdf
  (let [m (reduce #(.multiply %1 %2) (map first xs))]
    (reduce #(.add %1 %2)
            (map (fn [[p a]]
                   (let [z (.divide m p)
                         [_ _ y] (extended-euclid p z)]
                     (.multiply (.multiply a y) z)))
                 xs))))

;; I have no idea why I need the largest negative number instead of
;; the smallest positive number.
(defn largest-negative-multiple [x m]
  (if (< 0 x)
    (loop [x x]
      (if (< 0 x)
        (recur (.subtract x m))
        x))
    (loop [x x]
      (if (< 0 x)
        (.subtract x m)
        (recur (.add x m))))))

(defn part2 [input]
  (let [[_ buses] (parse-input input)
        m (reduce (fn [x y] (.multiply x y)) (map first buses))
        x (solve-modulo-equations buses)]
    (.abs (largest-negative-multiple x m))))
