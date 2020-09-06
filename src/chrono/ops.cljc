(ns chrono.ops
  (:require [chrono.new-ops :as ops]
            [chrono.util :as u]))


(defn leap-year? [y]
  (and (zero? (rem y 4))
       (or (pos? (rem y 100))
           (zero? (rem y 400)))))


(defn days-in-month [{m :month, y :year, :as v}]
  (cond
    (some nil? [m y]) ##Inf
    (contains? #{4 6 9 11} m) 30
    (and (leap-year? y) (= 2 m)) 29
    (= 2 m) 28
    :else 31))


(def calendar
  (array-map
   :ms    [0 1 '.. 999]
   :sec   [0 1 '.. 59]
   :min   [0 1 '.. 59]
   :hour  [0 1 '.. 23]
   :day   [1 2 '.. days-in-month]
   :month [1 2 '.. 12]
   :year  [##-Inf '.. -2 -1 1 2 '.. ##Inf]))


(defmethod ops/definition :default-type [_] calendar)


(defn to-new-fmt [value]
  (let [default-value (into (with-meta
                              (if (contains? value :year)
                               {:year -1}
                               {})
                              (meta value))
                            (map (juxt identity (partial ops/get-min-value {})))
                            (keys (dissoc value :tz :year)))]
    (cond-> value
      (contains? value :month)                        (update :month dec)
      (contains? value :day)                          (update :day dec)
      (not-any? (comp (every-pred int? neg?) val) value) (-> (with-meta {:delta true})
                                                          (->> (ops/plus default-value)))
      (contains? value :tz)                           (assoc :tz {:hour (:tz value)}))))

(defn to-old-fmt [value]
  (cond-> value
    (contains? value :tz)
    (update :tz :hour)))

(defn from-old-fmt [f & args]
  (apply f (map to-new-fmt args)))

(defn in-old-fmt [f & args]
  (to-old-fmt (apply from-old-fmt f args)))

(defn to-tz [value tz]
  (cond->> value
    (some? tz)
    (in-old-fmt #(ops/to-delta % ^:tz{:hour tz}))))

(defn to-utc [value]
  (to-tz value 0))

(def eq? (partial from-old-fmt ops/eq?))
(def not-eq? (partial from-old-fmt ops/not-eq?))
(def gt (partial from-old-fmt ops/gt?))
(def lt (partial from-old-fmt ops/lt?))
(def gte (partial from-old-fmt ops/gte?))
(def lte (partial from-old-fmt ops/lte?))

(defn plus [x & args]
  (apply in-old-fmt ops/plus x (map #(with-meta % {:delta true}) args)))

(def minus (partial in-old-fmt ops/minus))

(def normalize (partial in-old-fmt ops/normalize))

(def to-normalized-utc (comp normalize #(to-tz % 0)))

(def cmp (partial from-old-fmt ops/cmp))
