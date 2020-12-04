(ns aoc.2020.day4
  (:require [clojure.string :as str]))

(defn parse-passport [file]
  (let [fields (str/split file #"\s")
        pairs (map (fn [field]
                     (let [[key value] (.split field ":")]
                       {(keyword key) value}))
                   fields)]
    (apply merge pairs)))

(defn parse-batch-file [input]
  (map parse-passport (.split input "\n\n")))

(defn count-valid [input valid-fn]
  (count (filter valid-fn (parse-batch-file input))))

(defn all-fields-present? [passport]
  (every? #(contains? passport %) [:ecl :pid :eyr :hcl :byr :iyr :hgt]))

(defn part1 [input]
  (count-valid input all-fields-present?))

(defn part2 [input]
  (letfn [(valid-year? [str min max]
            (and (re-matches #"\d{4}" str)
                 (<= min (Integer/parseInt str) max)))
          (valid-length? [str]
            (when-let [[_ l m] (re-matches #"(\d+)(in|cm)" str)]
              (let [l (Integer/parseInt l)]
                (case m
                  "in" (<= 59 l 76)
                  "cm" (<= 150 l 193)))))
          (valid-hair-color? [str]
            (re-matches #"#[0-9a-f]{6}" str))
          (valid-eye-color? [str]
            (some #{str} ["amb" "blu" "brn" "gry" "grn" "hzl" "oth"]))
          (valid-passport-id? [str]
            (re-matches #"[0-9]{9}" str))]
    (count-valid input
                 (fn [passport]
                   (and (all-fields-present? passport)
                        (let [{:keys [ecl pid eyr hcl byr iyr hgt]} passport]
                          (and (valid-year? byr 1920 2002)
                               (valid-year? iyr 2010 2020)
                               (valid-year? eyr 2020 2030)
                               (valid-length? hgt)
                               (valid-hair-color? hcl)
                               (valid-eye-color? ecl)
                               (valid-passport-id? pid))))))))
