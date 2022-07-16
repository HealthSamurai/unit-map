(ns unit-map.systems.date-time.now
  (:require [unit-map.core :as umap]
            [unit-map.systems.date-time.defs]))

;; TODO:
;; - What if someone wants to use :second & :nanosecond?
;; - Add am pm period support


(defn tz-offset
  ([]
   (tz-offset {:min (-> #?(:clj  (java.util.Date.)
                           :cljs (js/Date.))
                        .getTimezoneOffset -)}))
  ([offset]
   (umap/add-delta {} offset) #_"TODO: replace with normalize"))


(defn local []
  (let [now #?(:clj  (java.util.Date.)
               :cljs (js/Date.))]
    (assoc {}
           :year  (-> (.getYear now)
                      (+ 1900))
           :month (umap/useq-nth (get-in @umap/registry-atom [:useqs :month :year])
                                 {}
                                 (.getMonth now))
           :day   (.getDate now)
           :hour  (.getHours now)
           :min   (.getMinutes now)
           :sec   (.getSeconds now)
           :ms    (-> (.getTime now)
                      (rem 1000))
           :tz    (tz-offset {:min (- (.getTimezoneOffset now))}))))


(defn utc []
  (let [now (local)]
    (assoc (umap/subtract-delta now (:tz now)) #_"TODO: replace with remove deltas"
           :tz {:hour 0})))


(defn today []
  (select-keys (local) [:year :month :day :tz]))


(defn utc-today []
  (select-keys (utc) [:year :month :day :tz]))


(defn time []
  (select-keys (local) [:hour :min :sec :ms :tz]))


(defn utc-time []
  (select-keys (utc) [:hour :min :sec :ms :tz]))
