(ns aoc.2020.day2)

(defn parse-line [line]
  (let [[_ min max letter password] (re-matches #"(\d+)-(\d+) ([a-z]): ([a-z]+)" line)]
    {:min (Integer/parseInt min)
     :max (Integer/parseInt max)
     :letter (get letter 0)
     :password password}))

(defn parse-input [input]
  (map parse-line (.split input "\n")))

(defn count-valid [input valid-fn]
  (let [entries (parse-input input)]
    (count (filter valid-fn entries))))

(defn part1 [input]
  (count-valid input
               (fn [entry]
                 (let [{:keys [min max letter password]} entry
                       count (get (frequencies password) letter 0)] 
                   (<= min count max)))))

(defn part2 [input]
  (count-valid input
               (fn [entry]
                 (let [{:keys [min max letter password]} entry
                       [first second] (map #(get password (dec %)) [min max])]
                   (not (= (= letter first)
                           (= letter second)))))))
