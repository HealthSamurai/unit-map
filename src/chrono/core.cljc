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
;; (def = tz/=)
;; (def > tz/>)
;; (def >= tz/>=)
;; (def < tz/<)
;; (def <= tz/<=)





(def default-time {:year 0 :month 1 :day 1 :hour 0 :min 0 :sec 0})
(defn eq? [t t']
  (let [t (merge default-time t)
        t' (merge default-time t')]
    (and
     (= (:year t) (:year t'))
     (= (:month t) (:month t'))
     (= (:day t) (:day t'))
     (= (:hour t) (:hour t'))
     (= (:min t) (:min t'))
     (= (:sec t) (:sec t')))))

(defn date-convertable? [value in out]
  (eq?
   (parse value in)
   (parse (format (parse value in ) out) out)))

(defn date-valid? [value fmt]
  #?(:clj true
     :cljs (not (js/isNaN (.parse js/Date (format (parse value fmt) [:year "-" :month "-" :day]))))))
