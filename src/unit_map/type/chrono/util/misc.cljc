(ns unit-map.type.chrono.util.misc
  (:require [unit-map.ops :as ops]
            [unit-map.type.chrono.util.now :as now]))


;; TODO: tz fmt support
(def iso-fmt [:year "-" :month "-" :day "T" :hour ":" :min ":" :sec "." :ms])


(defn leap-year? [y]
  (and (zero? (rem y 4))
       (or (pos? (rem y 100))
           (zero? (rem y 400)))))


(defn days-in-month [{m :month, y :year}]
  (cond
    (some nil? [m y])            ##Inf
    (contains? #{4 6 9 11} m)    30
    (and (leap-year? y) (= 2 m)) 29
    (= 2 m)                      28
    :else                        31))


(def epoch {:year 1970 :day 1 :month 1})


(defn from-epoch [e]
  (ops/plus epoch ^:delta{:sec e}))


(defn seconds [d]
  (+ (* (dec (:day d)) 60 60 24)
     (* (:hour d) 60 60)
     (* (:min d) 60)
     (:sec d)))


(defn to-epoch [date]

  (let [years (range (:year epoch) (:year date))
        months (range 1 (:month date))]
    (-> date
        (dissoc :year :month)
        (update :day #(reduce (fn [days year]
                                (+ days (if (leap-year? year) 366 365))) % years))
        (update :day #(reduce (fn [days month]
                                (+ days (days-in-month {:month month :year (:year date)}))) % months))
        seconds)))


(defn timestamp
  ([]  (timestamp (now/utc)))
  ([t] (to-epoch t)))
