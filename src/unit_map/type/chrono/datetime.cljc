(ns unit-map.type.chrono.datetime
  (:require [unit-map.ops :as ops]
            [unit-map.type.chrono.util.misc :as um]))


(derive ::military :unit-map.type.chrono.date/date)
(derive ::am-pm :unit-map.type.chrono.date/date)


(def gregorian-military
  #unit-map/definition[:ms    [0 1 .. 999]
                       :sec   [0 1 .. 59]
                       :min   [0 1 .. 59]
                       :hour  [0 1 .. 23]
                       :day   [1 2 .. um/days-in-month]
                       :month [1 2 .. 12]
                       :year  [##-Inf .. -2 -1 1 2 .. ##Inf]])


(defmethod ops/definition ::military [_] gregorian-military)


(def gregorian-am-pm
  #unit-map/definition[:ms     [0 1 .. 999]
                       :sec    [0 1 .. 59]
                       :min    [0 1 .. 59]
                       :hour   [12 1 2 .. 11]
                       :period [:am :pm]
                       :day    [1 2 .. um/days-in-month]
                       :month  [1 2 .. 12]
                       :year   [##-Inf .. -2 -1 1 2 .. ##Inf]])


(defmethod ops/definition ::am-pm [_] gregorian-am-pm)
