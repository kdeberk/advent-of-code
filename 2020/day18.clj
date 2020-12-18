(ns aoc.2020.day18
  (:require [clojure.string :as str]))

(def basic-grammar
  {:open  [:str    "(" identity]
   :close [:str    ")" identity]
   :plus  [:str    "+" symbol]
   :mult  [:str    "*" symbol]
   :int   [:regexp #"[0-9]+" #(Integer/parseInt %)]})

(defn parse [text grammar]
  ;; Since all operators are left associative, e.g. 1 + 2 + 3 -> (1 + 2) + 3,
  ;; and the grammar is left-recursive, we avoid infinite loops by
  ;; parsing the text in reverse (we pop the tokens starting at the end of the string)
  (letfn [(read-rule [text rule]
            (when text
              (let [text       (str/trim text)
                    [t rule f] (grammar rule)]
                (case t
                  :regexp (let [[m x] (re-find (re-pattern (str "(" rule ")$")) text)]
                            [(f x) (subs text 0 (- (count text) (count m)))])
                  :seq    (if-let [[parsed text] (reduce (fn [[parsed text] rule]
                                                           (when-let [[p rest] (read-rule text rule)]
                                                             [(into [] (concat [p] parsed)) rest]))
                                                         [[] text]
                                                         (reverse rule))]
                            [(f parsed) text])
                  :one-of (some #(read-rule text %) rule)
                  :str    (when (str/ends-with? text rule)
                            [(f rule) (subs text 0 (- (count text) (count rule)))])))))]
    (first (read-rule text :start))))

(defn parse-input [input grammar]
  (map #(parse % grammar) (.split input "\n")))

(defn part1 [input]
  (let [grammar (assoc basic-grammar
                       :op      [:one-of [:plus :mult] identity]
                       :subexpr [:seq    [:open :expr1 :close] second]
                       :term    [:one-of [:subexpr :int] identity]
                       :expr2   [:seq    [:expr1 :op :term] (fn [[x op y]] (list op x y))]
                       :expr1   [:one-of [:expr2 :term] identity]
                       :start   [:one-of [:expr1] identity])]
    (reduce + (map eval (parse-input input grammar)))))

(defn part2 [input]
  (let [grammar (assoc basic-grammar
                       :subexpr [:seq    [:open :expr1 :close] second]
                       :term    [:one-of [:subexpr :int] identity]
                       :factor2 [:seq    [:factor1 :plus :term] (fn [[x op y]] (list op x y))]
                       :factor1 [:one-of [:factor2 :term] identity]
                       :expr2   [:seq    [:expr1 :mult :factor1] (fn [[x op y]] (list op x y))]
                       :expr1   [:one-of [:expr2 :factor1] identity]
                       :start   [:one-of [:expr1] identity])]
    (reduce + (map eval (parse-input input grammar)))))
