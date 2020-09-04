(ns chrono.type.date
  (:require [chrono.new-ops :as ops]))


(defn leap-year? [y]
  (and (zero? (rem y 4))
       (or (pos? (rem y 100))
           (zero? (rem y 400)))))


(defn days-in-month [{m :month, y :year}]
  (cond
    (some nil? [m y])                    ##Inf
    (contains? #{4 6 9 11} m) 30
    (and (leap-year? y) (= 2 m))      29
    (= 2 m)                           28
    :else                                31))


(def gregorian
  (array-map
   :day    [1 2 '.. days-in-month]
   :month  [1 2 '.. 12]
   :year   [##-Inf '.. -2 -1 1 2 '.. ##Inf]))


(defmethod ops/definition ::gregorian [_] gregorian)
