(ns aoc.2020.day13)

(defn parse-input [input]
  (let [[t bs] (.split input "\n")]
    [(Integer/parseInt t)
     (map (fn [[i b]]
            (let [p (biginteger (Integer/parseInt b))]
              [p (.mod (biginteger i) p)]))
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

(defn extended-euclid [^BigInteger a ^BigInteger b]
  (loop [[^BigInteger s ^BigInteger s'] [(biginteger 0) (biginteger 1)]
         [^BigInteger r ^BigInteger r'] [b a]
         [^BigInteger t ^BigInteger t'] [(biginteger 1) (biginteger 0)]]
    (if (not (zero? r))
      (let [q (.divide r' r)]
        (recur [(.subtract s' (.multiply q s)) s]
               [(.subtract r' (.multiply q r)) r]
               [(.subtract t' (.multiply q t)) t]))
      [r' s' t'])))

(defn ^BigInteger solve-modulo-equations [xs]
  ;; From: https://homepages.math.uic.edu/~leon/mcs425-s08/handouts/chinese_remainder.pdf
  (let [^BigInteger m (reduce (fn [^BigInteger a ^BigInteger b] (.multiply a b))
                              (map first xs))]
    (reduce (fn [^BigInteger a ^BigInteger b] (.add a b))
            (map (fn [[^BigInteger p ^BigInteger a]]
                   (let [z (.divide m p)
                         [_ _ y] (extended-euclid p z)]
                     (.multiply (.multiply a y) z)))
                 xs))))

;; I have no idea why I need the largest negative number instead of
;; the smallest positive number.
(defn largest-negative-multiple [^BigInteger x ^BigInteger m]
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
        m (reduce (fn [^BigInteger x ^BigInteger y] (.multiply x y)) (map first buses))
        x (solve-modulo-equations buses)]
    (.abs (largest-negative-multiple x m))))
