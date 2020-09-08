(ns chrono.type.datetime.date
  (:require [chrono.ops :as ops]
            [chrono.type.datetime.util.misc :as um]))


(def gregorian
  #chrono/definition[:day    [1 2 .. um/days-in-month]
                     :month  [1 2 .. 12]
                     :year   [##-Inf .. -2 -1 1 2 .. ##Inf]])


(defmethod ops/definition ::gregorian [_] gregorian)
