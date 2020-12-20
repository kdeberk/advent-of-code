(ns aoc.2020.day20
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defrecord tile [id cells borders])

(defn make-tile [id cells]
  (->tile id cells (calculate-borders cells)))

(defrecord grid [cells])

(defn make-grid [grid]
  (letfn [(strip-borders [cells]
            (into [] (map #(subvec % 1 (dec (count %)))
                          (subvec cells 1 (dec (count cells))))))]
    (let [grid (into {} (map (fn [[k v]]
                               [k (strip-borders (:cells v))])
                             grid))
          dim (int (Math/sqrt (count grid)))
          rows (for [y (range 0 dim)]
                 (let [ks (for [x (range 0 dim)] [y x])]
                   (map #(concat (apply concat %))
                        (apply map vector (map grid ks)))))]
      (->grid (vec (apply concat rows))))))

(defn calculate-borders [cells]
  (letfn [(to-int [bs]
            (apply + (map (fn [[i b]]
                            (* (int (Math/pow 2 i))
                               (case b
                                 \# 1
                                 \. 0)))
                          (map-indexed vector bs))))
          (get-all-borders [cells]
            (let [borders {:top (first cells)       :rtop (reverse (first cells))
                           :bottom (last cells)     :rbottom (reverse (last cells))
                           :right (map last cells)  :rright (reverse (map last cells))
                           :left  (map first cells) :rleft (reverse (map first cells))}
                  borders (map (fn [[k v]] [k (to-int v)]) borders)]
              (into {} (concat borders (map (fn [[k v]] [v k]) borders)))))]
    (get-all-borders cells)))

(defn parse-input [input]
  (letfn [(parse-id [title]
            (let [[_ n] (re-matches #"Tile (\d+):" title)]
              (Integer/parseInt n)))
          (parse-tile-entry [tile]
            (let [[title & lines] (.split tile "\n")]
              (make-tile (parse-id title)
                         (into [] (map vec lines)))))
          (parse-tiles [input]
            (map parse-tile-entry (.split input "\n\n")))]
    (parse-tiles input)))

(defn get-corners [tiles]
  (let [counts  (frequencies (mapcat #(vals (:borders %)) tiles))]
    (filter (fn [tile]
              (= 4 (count (filter #{1} (map counts (vals (:borders tile)))))))
            tiles)))

(defmulti rotate type)
(defmulti hflip type)
(defmulti vflip type)

(defmethod rotate tile [tile]
  (let [cells (into [] (map #(vec (reverse %))
                            (apply map vector (:cells tile))))]
    (assoc tile :cells cells :borders (calculate-borders cells))))

(defmethod rotate grid [grid]
  (let [cells (into [] (map #(vec (reverse %))
                            (apply map vector (:cells grid))))]
    (assoc grid :cells cells)))

(defmethod hflip tile [tile]
  (let [cells (into [] (map #(vec (reverse %)) (:cells tile)))]
    (assoc tile :cells cells :borders (calculate-borders cells))))

(defmethod hflip grid [grid]
  (let [cells (into [] (map #(vec (reverse %)) (:cells grid)))]
    (assoc grid :cells cells)))

(defmethod vflip tile [tile]  ;; vlaflip
  (let [cells (into [] (reverse (:cells tile)))]
    (assoc tile :cells cells :borders (calculate-borders cells))))

(defn transform [tile from to]
  (let [actions (case [from to]
                  ([:rtop :left]   [:right :top])    [rotate rotate rotate]
                  ([:rbottom :top] [:rright :left])  [rotate rotate]
                  ([:top :top]     [:left :left])    []
                  ([:rright :top]  [:rbottom :left]) [rotate vflip]
                  ([:right :left]  [:rtop :top])     [hflip]
                  ([:left :top]    [:top :left])     [rotate hflip]
                  ([:rleft :left]  [:bottom :top])   [vflip]
                  ([:rleft :top]   [:bottom :left])  [rotate])]
    (reduce #(%2 %1) tile actions)))

(defn solve-tile [[y x] grid tiles]
  (let [[dir border] (if (= x 0)
                       [:top  (:bottom (:borders (grid [(dec y) x])))]
                       [:left (:right (:borders (grid [y (dec x)])))])
        tile (first (filter #(some #{border} (keys (:borders %))) tiles))]
    (when tile
      (transform tile ((:borders tile) border) dir))))

(defn solve-tiles [tiles]
  (let [corners (get-corners tiles)
        start (first corners)
        tiles (into {} (map (fn [t] [(:id t) t]) tiles))
        dim   (int (Math/sqrt (count tiles)))]
    (loop [actions  [rotate rotate rotate vflip rotate rotate rotate]
           to-place (dissoc tiles (:id start))
           grid     {[0 0] start}
           y 0 x 1]
      (if (empty? to-place)
        (make-grid grid)
        (let [tile (solve-tile [y x] grid (vals to-place))]
          (cond
            ;; Couldn't solve board :(
            (and (not tile) (empty? actions))
            nil
            ;; Restart with a different initial tile rotation
            (not tile)
            (recur (rest actions) (dissoc tiles (:id start)) {[0 0] ((first actions) (grid [0 0]))} 0 1)
            ;; Next row
            (= dim (inc x))
            (recur actions (dissoc to-place (:id tile)) (assoc grid [y x] tile) (inc y) 0)
            ;; Next cell
            :else
            (recur actions (dissoc to-place (:id tile)) (assoc grid [y x] tile) y (inc x))))))))

(def monster
  ["                  # "
   "#    ##    ##    ###"
   " #  #  #  #  #  #   "])

(defn count-monsters [grid]
  (letfn [(all-indexes [s r]
            (loop [idx  0
                   idxs #{}]
              (if (< idx (count s))
                (recur (inc idx)
                       (if (re-find r (subs s idx))
                         (conj idxs idx)
                         idxs))
                idxs)))]
    (let [cells   (:cells grid)
          monster (map #(re-pattern (str "^" (str/replace % " " "."))) monster)]
      (reduce + (for [y (range 0 (inc (- (count cells) (count monster))))]
                  (let [idxss (map all-indexes (map str/join (subvec cells y (+ y 3))) monster)]
                    (count (reduce set/intersection idxss))))))))

(defn part1 [input]
  (let [tiles   (parse-input input)
        corners (get-corners tiles)]
    (reduce * (map :id corners))))

(defn part2 [input]
  (let [tiles    (parse-input input)
        grid     (solve-tiles tiles)
        g-hashes (count (filter #{\#} (flatten (:cells grid))))
        m-hashes (count (filter #{\#} (str/join (flatten monster))))]
    (loop [grid    grid
           actions [rotate rotate rotate rotate hflip rotate rotate rotate]]
      (let [n (count-monsters grid)]
        (if (< 0 n)
          (- g-hashes (* n m-hashes))
          (recur ((first actions) grid)
                 (rest actions)))))))
