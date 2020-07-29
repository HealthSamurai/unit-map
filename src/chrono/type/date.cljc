(ns chrono.type.date
  (:require [chrono.new-ops :as ops]))

(defn leap-year? [y]
  (and (zero? (rem y 4))
       (or (pos? (rem y 100))
           (zero? (rem y 400)))))

(defn days-in-month [{m :month, y :year}]
  (cond
    (contains? #{:apr :jun :sep :nov} m) 30
    (and (leap-year? y) (= :feb m)) 29
    (= :feb m) 28
    :else 31))

(def gregorian
  (array-map
   :day    [1 2 '.. days-in-month]
   :month  [:jan :feb :mar :apr :may :jun :jul :aug :sep :oct :nov :dec]
   :year   [##-Inf '.. -2 -1 1 2 '.. ##Inf]))

(defmethod ops/type ::gregorian [_] gregorian)
