(defpackage #:utils
  (:use :cl :str)
  (:export :read-input :read-lines :read-numbers :stringcase :partial :if-let :read-grid :push-or-inc))

(defpackage #:matrix
  (:use :cl)
  (:export :*-vector :*-matrix :exponent))

(defpackage #:day1
  (:use :cl :parachute))

(defpackage #:day2
  (:use :cl :parachute))

(defpackage #:day3
  (:use :cl :parachute))

(defpackage #:day4
  (:use :cl :parachute))

(defpackage #:day5
  (:use :cl :parachute))

(defpackage #:day6
  (:use :cl :parachute))

(defpackage #:day6-matrix
  (:use :cl :parachute))

(defpackage #:day7
  (:use :cl :parachute))

(defpackage #:day8
  (:use :cl :parachute))

(defpackage #:day9
  (:use :cl :parachute))

(defpackage #:day10
  (:use :cl :parachute))

(defpackage #:day11
  (:use :cl :parachute))

(defpackage #:day12
  (:use :cl :parachute))

(defpackage #:day13
  (:use :cl :parachute))

(defpackage #:day14
  (:use :cl :parachute))

(defpackage #:day14-pairs
  (:use :cl :parachute))

(defpackage #:day15
  (:use :cl :parachute))

(defpackage #:day16
  (:use :cl :parachute))

(asdf:defsystem "aoc2021"
  :description "Code for the Advent of Code 2021 Challenge"
  :depends-on (:parachute :cl-ppcre :str)
  :components ((:file "utils")
               (:file "matrix")
               (:file "day1" :depends-on ("utils"))
               (:file "day2" :depends-on ("utils"))
               (:file "day3" :depends-on ("utils"))
               (:file "day4" :depends-on ("utils"))
               (:file "day5" :depends-on ("utils"))
               (:file "day6" :depends-on ("utils"))
               (:file "day6-matrix" :depends-on ("day6" "utils" "matrix"))
               (:file "day7" :depends-on ("utils"))
               (:file "day8" :depends-on ("utils"))
               (:file "day9" :depends-on ("utils"))
               (:file "day10" :depends-on ("utils"))
               (:file "day11" :depends-on ("utils"))
               (:file "day12" :depends-on ("utils"))
               (:file "day13" :depends-on ("utils"))
               (:file "day14" :depends-on ("utils"))
               (:file "day14-pairs" :depends-on ("utils"))
               (:file "day15" :depends-on ("utils"))
               (:file "day16" :depends-on ("utils"))))
