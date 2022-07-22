(ns unit-map.systems.date-time.now
  (:require [unit-map.core :as umap]
            [unit-map.impl.registry :as registry]
            [unit-map.impl.system :as system]))

;; TODO:
;; - What if someone wants to use :second & :nanosecond?
;; - Add am pm period support


(defn tz-offset
  ([registry]
   (tz-offset registry
              {:min (-> #?(:clj  (java.util.Date.)
                           :cljs (js/Date.))
                        .getTimezoneOffset -)}))
  ([registry offset]
   (umap/add-delta registry {} offset) #_"TODO: replace with normalize"))


(defn local [registry]
  (let [now #?(:clj  (java.util.Date.)
               :cljs (js/Date.))]
    (assoc {}
           :year  (-> (.getYear now)
                      (+ 1900))

           :month (system/useq-nth (registry/useq registry :month :year)
                                   {}
                                   (.getMonth now))
           :day   (.getDate now)
           :hour  (.getHours now)
           :min   (.getMinutes now)
           :sec   (.getSeconds now)
           :ms    (-> (.getTime now)
                      (rem 1000))
           :tz    (tz-offset registry {:min (- (.getTimezoneOffset now))}))))


(defn utc [registry]
  (let [now (local registry)]
    (assoc (umap/subtract-delta registry now (:tz now)) #_"TODO: replace with remove deltas"
           :tz {:hour 0})))


(defn today [registry]
  (select-keys (local registry) [:year :month :day :tz]))


(defn utc-today [registry]
  (select-keys (utc registry) [:year :month :day :tz]))


(defn time [registry]
  (select-keys (local registry) [:hour :min :sec :ms :tz]))


(defn utc-time [registry]
  (select-keys (utc registry) [:hour :min :sec :ms :tz]))
