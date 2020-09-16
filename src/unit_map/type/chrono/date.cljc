(ns unit-map.type.chrono.date
  (:require [unit-map.ops :as ops]
            [unit-map.type.chrono.util.misc :as um]))


(def gregorian
  #unit-map/definition[:day    [1 2 .. um/days-in-month]
                       :month  [1 2 .. 12]
                       :year   [##-Inf .. -2 -1 1 2 .. ##Inf]])


(defmethod ops/definition ::gregorian [_] gregorian)
