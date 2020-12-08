(ns aoc.2020.day8)

(defrecord machine [ip acc])

(defn execute-dispatch-fn [[ins jmp] machine]
  ins)

(defmulti execute #'execute-dispatch-fn)

(defmethod execute :nop [[_ _] machine]
  (update machine :ip inc))

(defmethod execute :acc [[_ acc] machine]
  (assoc machine
         :acc (+ (:acc machine) acc)
         :ip (inc (:ip machine))))

(defmethod execute :jmp [[_ jmp] machine]
  (update machine :ip + jmp))

(defn parse-instructions [input]
  (vec (map (fn [line]
              (let [[ins jmp] (.split line " ")]
                [(keyword ins) (Integer/parseInt jmp)]))
            (.split input "\n"))))

(defn part1 [input]
  (let [machine (->machine 0 0)
        instructions (parse-instructions input)]
    (letfn [(run-machine [machine visited]
              (if (visited (:ip machine))
                machine
                (recur (execute (instructions (:ip machine)) machine)
                       (conj visited (:ip machine)))))]
      (:acc (run-machine machine #{})))))

(defn fix-instructions [instructions]
  (letfn [(fix [i fixed]
            (if (= i (count instructions))
              fixed
              (let [[ins x] (instructions i)]
                (case ins
                  :jmp (recur (inc i) (conj fixed (assoc instructions i [:nop x])))
                  :nop (recur (inc i) (conj fixed (assoc instructions i [:jmp x])))
                  (recur (inc i) fixed)))))]
    (fix 0 [])))

(defn part2 [input]
  (let [instructions (parse-instructions input)
        pairs (map (fn [modified]
                     [(->machine 0 0) modified])
                   (fix-instructions instructions))]
    (letfn [(run-pair [[machine instructions]]
              [(execute (instructions (:ip machine)) machine)
               instructions])
            (run-pairs [pairs]
              (let [terminated (filter (fn [[machine _]]
                                         (= (:ip machine) (count instructions)))
                                       pairs)]
                (if (< 0 (count terminated))
                  terminated
                  (recur (map run-pair pairs)))))]
      (map #(:acc (first %))
           (run-pairs pairs)))))
