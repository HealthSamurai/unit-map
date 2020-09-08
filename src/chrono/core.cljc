(ns chrono.core
  (:require [chrono.ops :as ops]
            [chrono.type.datetime.util.now :as now]
            [chrono.io :as io])
  (:refer-clojure :exclude [+ - = > >= < <= not= format compare]))


;; TODO: move this epoch logic to time type utils


(def epoch {:year 1970 :day 1 :month 1})


(defn from-epoch [e]
  (ops/plus epoch ^:delta{:sec e}))


(defn leap-year? [y]
  (and (zero? (rem y 4))
       (or (pos? (rem y 100))
           (zero? (rem y 400)))))


(defn days-in-month [{m :month, y :year, :as v}]
  (cond
    (some nil? [m y]) ##Inf
    (contains? #{4 6 9 11} m) 30
    (and (leap-year? y) (clojure.core/= 2 m)) 29
    (clojure.core/= 2 m) 28
    :else 31))


(defn seconds [d]
  (clojure.core/+ (* (dec (:day d)) 60 60 24)
                  (* (:hour d) 60 60)
                  (* (:min d) 60)
                  (:sec d)))


(defn to-epoch [date]

  (let [years (range (:year epoch) (:year date))
        months (range 1 (:month date))]
    (-> date
        (dissoc :year :month)
        (update :day #(reduce (fn [days year]
                                (clojure.core/+ days (if (leap-year? year) 366 365))) % years))
        (update :day #(reduce (fn [days month]
                                (clojure.core/+ days (days-in-month {:month month :year (:year date)}))) % months))
        seconds)))


(defn timestamp
  ([]  (timestamp (now/utc)))
  ([t] (to-epoch t)))


(def + ops/plus)
(def - ops/minus)
(def = ops/eq?)
(def not= ops/not-eq?)
(def eq? ops/eq?)
(def > ops/gt?)
(def >= ops/gte?)
(def < ops/lt?)
(def <= ops/lte?)
(def compare ops/cmp)
(def difference ops/difference)
(def normalize ops/normalize)
(def convertable? io/convertable?)
(def valid? io/valid?)


(def parse io/parse)
(def format io/format)
