(ns chrono.core
  (:require [chrono.util :as util]
            [chrono.tz :as tz]
            [chrono.now :as now]
            [chrono.io :as io]
            [clojure.string :as str]))

(defn datetime [t]
  (merge {:type :datetime
          :year 1900
          :month 1
          :day 1} t))

(def parse io/parse)
(def format io/format)

(defn timestamp [t])

(defn diff [t t'])

(def normalize tz/normalize)

(def + tz/+)
(def = tz/=)
(def > tz/>)
(def >= tz/>=)
(def < tz/<)
(def <= tz/<=)
