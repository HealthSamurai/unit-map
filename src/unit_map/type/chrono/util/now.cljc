(ns unit-map.type.chrono.util.now
  (:require [unit-map.ops :as ops]))

;; TODO:
;; - What if someone wants to use :second & :nanosecond?
;; - Add am pm period support


(defn tz-offset [& [value offset]]
  (let [offset (or offset
                   {:min (-> #?(:clj  (java.util.Date.)
                                :cljs (js/Date.))
                             .getTimezoneOffset -)})]
    (ops/normalize (with-meta offset (ops/make-delta-type value :tz)))))


(defn local [& [value-type]]
  (let [now #?(:clj  (java.util.Date.)
               :cljs (js/Date.))
        typed-value (cond-> {} (some? value-type) (with-meta {value-type true}))]
    (assoc typed-value
           :year  (-> (.getYear now)
                      (+ 1900))
           :month (->> (.getMonth now)
                       (ops/sequence-nth (ops/unit-definition typed-value :month) typed-value))
           :day   (.getDate now)
           :hour  (.getHours now)
           :min   (.getMinutes now)
           :sec   (.getSeconds now)
           :ms    (-> (.getTime now)
                      (rem 1000))
           :tz    (->> {:min (-> now .getTimezoneOffset -)}
                       (tz-offset typed-value)))))


(defn utc [& [value-type]]
  (let [now     (local value-type)
        tz-meta (meta (:tz now))]
    (assoc (ops/remove-deltas now)
           :tz (with-meta {:hour 0} tz-meta))))


(defn today [& [value-type]]
  (select-keys (local value-type) [:year :month :day :tz]))


(defn utc-today [& [value-type]]
  (select-keys (utc value-type) [:year :month :day :tz]))


(defn now [& [value-type]]
  (select-keys (local value-type) [:hour :min :sec :ms :tz]))


(defn utc-now [& [value-type]]
  (select-keys (utc value-type) [:hour :min :sec :ms :tz]))
