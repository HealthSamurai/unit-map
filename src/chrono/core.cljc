(ns chrono.core
  (:require [chrono.util :as util]))

(defn datetime [t]
  (merge {:type :datetime
          :year 1900
          :month 1
          :day 1} t))

(defn parse [s format])

(defn format [t format])

(defn timestamp [t])

(defn diff [t t'])

(defn plus [t i]
  (util/normalize
   (merge-with + t i)))

(defn to-tz [t tz])
