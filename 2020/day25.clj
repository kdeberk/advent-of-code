(ns aoc.2020.day25)

(defn part1 [a, b]
  (let [prime   20201227
        subject 7
        [pub key] (loop [public-a subject
                         public-b subject
                         loop    1]
                    (cond
                      (= public-a a) [b loop]
                      (= public-b b) [a loop]
                      :else (recur (mod (* public-a subject) prime)
                                   (mod (* public-b subject) prime)
                                   (inc loop))))]
    (reduce (fn [x _]
              (mod (* x pub) prime))
            1
            (range 0 key))))
