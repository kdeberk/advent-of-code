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
               (:file "day6" :depends-on ("utils" "matrix"))
               (:file "day7" :depends-on ("utils"))))

(defpackage #:utils
  (:use :cl :str)
  (:export :read-input :read-lines :read-numbers :stringcase :partial))

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

(defpackage #:day7
  (:use :cl :parachute))
