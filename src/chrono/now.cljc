(ns chrono.now
  (:require [chrono.ops :as ops]))


;; TODO:
;; - unhardcode :year :month :day :hour :min :sec :ms keywords?
;;   If someone wants to use :second & :nanosecond for example
;; - Add am pm period support


(defn tz-offset
  ([] (tz-offset #?(:clj  {:sec (-> (java.time.ZonedDateTime/now) bean :offset bean :totalSeconds)}
                    :cljs {:min (-> (js/Date.) .getTimezoneOffset -)})))
  ([offset] (-> (with-meta offset {:tz true})
                ops/normalize
                ops/strip-zeros)))


(defn local [& [value-type]]
  (let [now #?(:clj  (bean (java.time.ZonedDateTime/now))
               :cljs (js/Date.))
        typed-value (cond-> {} (some? value-type) (with-meta {value-type true}))]
    (assoc typed-value
           :year  #?(:clj  (-> now :year)
                     :cljs (-> now .getFullYear))
           :month (->> #?(:clj  (-> now :month bean :value dec)
                          :cljs (-> now .getMonth))
                       (ops/sequence-nth (ops/unit-definition typed-value :month) typed-value))
           :day   #?(:clj  (-> now :dayOfMonth)
                     :cljs (-> now .getDate))
           :hour  #?(:clj  (-> now :hour)
                     :cljs (-> now .getHours))
           :min   #?(:clj  (-> now :minute)
                     :cljs (-> now .getMinutes))
           :sec   #?(:clj  (-> now :second)
                     :cljs (-> now .getSeconds))
           :ms    #?(:clj  (-> now :nano (quot 1000000))
                     :cljs (-> now .getMilliseconds))
           :tz    (tz-offset #?(:clj  {:sec (-> now :offset bean :totalSeconds)}
                                :cljs {:min (-> now .getTimezoneOffset -)})))))


(defn utc [& [value-type]]
  (let [now #?(:clj  (bean (java.time.ZonedDateTime/now java.time.ZoneOffset/UTC))
               :cljs (js/Date.))
        typed-value (cond-> {} (some? value-type) (with-meta {value-type true}))]
    (assoc typed-value
           :year  #?(:clj  (-> now :year)
                     :cljs (-> now .getUTCFullYear))
           :month (->> #?(:clj  (-> now :month bean :value dec)
                          :cljs (-> now .getUTCMonth))
                       (ops/sequence-nth (ops/unit-definition typed-value :month) typed-value))
           :day   #?(:clj  (-> now :dayOfMonth)
                     :cljs (-> now .getUTCDate))
           :hour  #?(:clj  (-> now :hour)
                     :cljs (-> now .getUTCHours))
           :min   #?(:clj  (-> now :minute)
                     :cljs (-> now .getUTCMinutes))
           :sec   #?(:clj  (-> now :second)
                     :cljs (-> now .getUTCSeconds))
           :ms    #?(:clj  (-> now :nano (quot 1000000))
                     :cljs (-> now .getUTCMilliseconds))
           :tz    {:hour 0})))


(defn today [& [value-type]]
  (select-keys (local value-type) [:year :month :day :tz]))


(defn utc-today [& [value-type]]
  (select-keys (utc value-type) [:year :month :day :tz]))


(defn now [& [value-type]]
  (select-keys (local value-type) [:hour :min :sec :ms :tz]))


(defn utc-now [& [value-type]]
  (select-keys (utc value-type) [:hour :min :sec :ms :tz]))
