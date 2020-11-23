(ns unit-map.type.chrono.datetime
  (:require [unit-map.ops :as ops]
            [unit-map.type.chrono.date :as date]
            [unit-map.type.chrono.time :as time]))


(def gregorian-military (ops/merge-definitions time/military date/gregorian))


(defmethod ops/definition ::military [_] gregorian-military)


(def gregorian-am-pm (ops/merge-definitions time/am-pm date/gregorian))


(defmethod ops/definition ::am-pm [_] gregorian-am-pm)
