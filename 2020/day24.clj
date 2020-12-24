(ns aoc.2020.day24)

(defn parse-input [input]
  (map (fn [line]
         (loop [line line
                path []]
           (cond
             (empty? line) path
             (= \e (first line))        (recur (rest line) (conj path :east))
             (= \w (first line))        (recur (rest line) (conj path :west))
             (= '(\s \e) (take 2 line)) (recur (drop 2 line) (conj path :southeast))
             (= '(\n \e) (take 2 line)) (recur (drop 2 line) (conj path :northeast))
             (= '(\s \w) (take 2 line)) (recur (drop 2 line) (conj path :southwest))
             (= '(\n \w) (take 2 line)) (recur (drop 2 line) (conj path :northwest)))))
       (.split input "\n")))

(defn flip-tiles [paths]
  (letfn [(follow-path [path]
            (reduce (fn [[x y] step]
                      (case step
                        :east [(+ x 2) y]
                        :west [(- x 2) y]
                        :northeast [(inc x) (inc y)]
                        :southeast [(inc x) (dec y)]
                        :northwest [(dec x) (inc y)]
                        :southwest [(dec x) (dec y)]))
                    [0 0]
                    path))]
    (reduce (fn [seen path]
                (let [dst (follow-path path)]
                  (case (get seen dst :white)
                    :white (assoc seen dst :black)
                    :black (assoc seen dst :white))))
              {}
              paths)))

(defn part1 [input]
  (let [paths (parse-input input)]
    (count (filter #{:black} (vals (flip-tiles paths))))))

(def neighbors [[-2 0] [2 0] [-1 -1] [-1 1] [1 -1] [1 1]])

(defn part2 [input]
  (letfn [(count-black-neighbors [loc tiles]
            (count (filter #(= :black (tiles %))
                           (map #(mapv + loc %) neighbors))))
          (next-color [loc tiles]
            (let [color       (get tiles loc :white)
                  n-neighbors (count-black-neighbors loc tiles)]
              (cond (and (= :black color) (#{0 3 4 5 6} n-neighbors)) :white
                    (and (= :white color) (= 2 n-neighbors))          :black
                    :else color)))
          (next-floor [tiles]
            (into {} (let [black-keys  (map first (filter (fn [[_ v]] (= :black v)) tiles))
                           [minx miny] (map #(- % 2) (apply map min black-keys))
                           [maxx maxy] (map #(+ % 2) (apply map max black-keys))]
                       (for [x (range minx (inc maxx))
                             y (range miny (inc maxy))]
                         [[x y] (next-color [x y] tiles)]))))]
    (let [paths (parse-input input)
          floor (flip-tiles paths)]
      (reduce (fn [floor i] (next-floor floor))
              floor
              (range 0 100)))))
