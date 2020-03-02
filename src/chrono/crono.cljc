(ns chrono.crono
  (:require [chrono.core :as ch]
            [chrono.now :as now]))

(def needed-for
  {:month [:yaer :month]
   :day [:year :month :day]
   :hour [:year :month :day :hour]
   :min [:year :month :day :hour :min]})

(defn next-time-assumption [current-time {every :every at :at}])

(defn next-time
  ([cfg] (next-time (now/utc) cfg))
  ([current-time {every :every at :at :as when}]
   (let [assumptions (map #(merge (select-keys current-time (get needed-for every)) %) (if (map? at) [at] at))]
     (if (nil? (first (filter #(ch/< current-time %) assumptions)))
       (ch/+ (first assumptions) {every 1})
       (first (filter #(ch/< current-time %) assumptions))))))

(defn now?
  ([cfg] (now? (now/utc) cfg))
  ([current-time {every :every until :until :as when}]
   (if until
     (let [utmost-time (merge (select-keys current-time (get needed-for every)) until)]
       (ch/< current-time utmost-time))
     true)))

(comment

  (= {:year 2020 :month 1 :day 1 :hour 12}
     (next-time {:year 2020 :month 1 :day 1 :hour 11}
                {:every :day :at {:hour 12}}))

  (= {:year 2020 :month 1 :day 2 :hour 12}
     (next-time {:year 2020 :month 1 :day 1 :hour 12 :min 10}
                {:every :day :at {:hour 12}}))

  (= {:year 2020 :month 1 :day 1 :hour 14}
     (next-time-2 {:year 2020 :month 1 :day 1 :hour 13}
                  {:every :day :at [{:hour 12}
                                    {:hour 14}]}))

  (= {:year 2020 :month 1 :day 1 :hour 10 :min 30}
     (next-time-2 {:year 2020 :month 1 :day 1 :hour 10 :min 13}
                  {:every :hour :at [{:min 0} {:min 30}]}))

  (= {:year 2020 :month 1 :day 1 :hour 12}
     (next-time-2 {:year 2020 :month 1 :day 1 :hour 11 :min 43}
                  {:every :hour :at [{:min 0} {:min 30}]}))

  (= true
     (now? {:year 2020 :month 1 :day 1 :hour 12 :min 31}
           {:every :day
            :at {:hour 12}
            :until {:hour 12 :min 30}}))

  )
