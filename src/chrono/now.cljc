(ns chrono.now
  (:require [chrono.ops :as ops]))

(defn tz-offset []
  #?(:clj (-> (java.time.ZonedDateTime/now)
              .getOffset
              .getTotalSeconds
              (->> (hash-map :sec))
              ops/normalize)
     :cljs (-> (js/Date.)
               .getTimezoneOffset
               -
               (->> (hash-map :min))
               ops/normalize)))

(defn local []
  (let [now (#?(:clj  java.time.LocalDateTime/now
                :cljs js/Date.))]
    {:year  #?(:clj  (-> now .getYear)
               :cljs (-> now .getFullYear))
     :month #?(:clj  (-> now .getMonthValue)
               :cljs (-> now .getMonth inc))
     :day   #?(:clj  (-> now .getDayOfMonth)
               :cljs (-> now .getDate))
     :hour  #?(:clj  (-> now .getHour)
               :cljs (-> now .getHours))
     :min   #?(:clj  (-> now .getMinute)
               :cljs (-> now .getMinutes))
     :sec   #?(:clj  (-> now .getSecond)
               :cljs (-> now .getSeconds))
     :ms    #?(:clj  (-> now .getNano (/ 1000000))
               :cljs (-> now .getMilliseconds))}))

(defn utc []
  (let [now #?(:clj  (java.time.LocalDateTime/ofInstant
                      (java.time.Instant/now)
                      java.time.ZoneOffset/UTC)
               :cljs (js/Date.))]
    {:year  #?(:clj  (-> now .getYear)
               :cljs (-> now .getUTCFullYear))
     :month #?(:clj  (-> now .getMonthValue)
               :cljs (-> now .getUTCMonth inc))
     :day   #?(:clj  (-> now .getDayOfMonth)
               :cljs (-> now .getUTCDate))
     :hour  #?(:clj  (-> now .getHour)
               :cljs (-> now .getUTCHours))
     :min   #?(:clj  (-> now .getMinute)
               :cljs (-> now .getUTCMinutes))
     :sec   #?(:clj  (-> now .getSecond)
               :cljs (-> now .getUTCSeconds))
     :ms    #?(:clj  (-> now .getNano (/ 1000000))
               :cljs (-> now .getUTCMilliseconds))}))

(defn today []
  (select-keys (local) [:year :month :day]))

(defn utc-today []
  (select-keys (utc) [:year :month :day]))
