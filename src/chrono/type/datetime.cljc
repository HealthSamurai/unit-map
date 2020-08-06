(ns chrono.type.datetime
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

(def gregorian-military
  (array-map
   :ms    [0 1 '.. 999]
   :sec   [0 1 '.. 59]
   :min   [0 1 '.. 59]
   :hour  [0 1 '.. 23]
   :day   [1 2 '.. days-in-month]
   :month [:jan :feb :mar :apr :may :jun :jul :aug :sep :oct :nov :dec]
   :year  [##-Inf '.. -2 -1 1 2 '.. ##Inf]))

(defmethod ops/definition ::military [_] gregorian-military)


(def gregorian-am-pm
  (array-map
   :ms     [0 1 '.. 999]
   :sec    [0 1 '.. 59]
   :min    [0 1 '.. 59]
   :hour   [12 1 2 '.. 11]
   :period [:am :pm]
   :day    [1 2 '.. days-in-month]
   :month  [:jan :feb :mar :apr :may :jun :jul :aug :sep :oct :nov :dec]
   :year   [##-Inf '.. -2 -1 1 2 '.. ##Inf]))

(defmethod ops/definition ::am-pm [_] gregorian-am-pm)
