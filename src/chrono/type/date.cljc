(ns chrono.type.date
  (:require [chrono.new-ops :as ops]
            [chrono.util :as u]))

(def gregorian
  (array-map
   :day    [1 2 '.. u/days-in-month]
   :month  [:jan :feb :mar :apr :may :jun :jul :aug :sep :oct :nov :dec]
   :year   [##-Inf '.. -2 -1 1 2 '.. ##Inf]))

(defmethod ops/type ::gregorian [_] gregorian)
