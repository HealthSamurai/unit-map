(ns chrono.type.datetime.datetime
  (:require [chrono.ops :as ops]
            [chrono.type.datetime.util.misc :as um]))


(def gregorian-military
  #chrono/definition[:ms    [0 1 .. 999]
                     :sec   [0 1 .. 59]
                     :min   [0 1 .. 59]
                     :hour  [0 1 .. 23]
                     :day   [1 2 .. um/days-in-month]
                     :month [1 2 .. 12]
                     :year  [##-Inf .. -2 -1 1 2 .. ##Inf]])


(defmethod ops/definition ::military [_] gregorian-military)


(def gregorian-am-pm
  #chrono/definition[:ms     [0 1 .. 999]
                     :sec    [0 1 .. 59]
                     :min    [0 1 .. 59]
                     :hour   [12 1 2 .. 11]
                     :period [:am :pm]
                     :day    [1 2 .. um/days-in-month]
                     :month  [1 2 .. 12]
                     :year   [##-Inf .. -2 -1 1 2 .. ##Inf]])


(defmethod ops/definition ::am-pm [_] gregorian-am-pm)
