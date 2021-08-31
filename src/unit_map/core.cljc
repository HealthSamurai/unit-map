(ns unit-map.core
  (:require [unit-map.ops :as op]
            [unit-map.type.chrono.datetime :as dt]
            [unit-map.type.chrono.date :as d]
            [unit-map.type.chrono.time :as t]
            [unit-map.type.chrono.util.misc :as um]
            [clojure.set]))


(defonce systems (atom {}))


(defmacro defsys [sys-name units]
  (let [sys `(def ~sys-name ~units)]
    (swap! systems assoc sys-name units)
    sys))


(defmacro defseq [unit unit-seq])


(defn get-units [unit-map]
  (->> unit-map
       (mapcat (fn [[k v]]
                 (if (map? v)
                   (get-units v)
                   [k])))
       set))


(defn guess-sys [unit-map]
  (->> @systems
       vals
       (filter (comp (partial clojure.set/subset? (get-units unit-map))
                     set))
       sort))
