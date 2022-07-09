(ns unit-map.systems.date-time.misc
  (:require [unit-map.core :as umap]
            [unit-map.systems.date-time.defs :as defs]))


;; TODO: tz fmt support
(def iso-fmt [:year "-" :month "-" :day "T" :hour ":" :min ":" :sec "." :ms])


(defn leap-year? [date] (defs/leap-year? date))


(defn days-in-month [date] (defs/days-in-month date))


(def epoch {:year 1970 :day 1 :month 1})


(defn from-epoch [e]
  (umap/add-delta epoch {:sec e}))


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
                                (+ days (if (leap-year? {:year year}) 366 365))) % years))
        (update :day #(reduce (fn [days month]
                                (+ days (days-in-month {:month month :year (:year date)}))) % months))
        seconds)))
