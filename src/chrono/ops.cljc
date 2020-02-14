(ns chrono.ops
  (:require [chrono.util :as u]))

(defn gen-norm [k k-next del m]
  (fn [x]
    (if-let [z (get x k)]
      (let [ds (quot z (+ m del))
            s  (or (get x k-next) m)
            r  (-> (rem z (+ m del))
                   (as-> r (cond-> r (zero? r) (+ m r))))]
        (if (>= z m)
          (assoc x k r, k-next (+ s ds))
          (assoc x k (- (+ del r) m), k-next (+ s ds -1))))
      x)))

(def normalize-ms (gen-norm :ms :sec 1000 0))
(def normalize-s  (gen-norm :sec :min 60 0))
(def normalize-mi (gen-norm :min :hour 60 0))
(def normalize-h  (gen-norm :hour :day 24 0))
(def normalize-m  (gen-norm :month :year 12 1))

(defn days-and-months [y m d]
  (if (<= 1 d 27)
    [y m d]
    (cond
      (> d 0)
      (let [num-days (u/days-in-month {:year y, :month m})
            dd (- d num-days)]
        (if (<= d num-days)
          [y m d]
          (if (= m 12)
            (days-and-months (inc y) 1 dd)
            (days-and-months y (inc m) dd))))

      (<= d 0)
      (let [[num-days ny nm] (if (= m 1)
                               [(u/days-in-month {:year (dec y), :month 12}) (dec y) 12]
                               [(u/days-in-month {:year y, :month (dec m)}) y (dec m)])
            dd (+ num-days d)]
        (if (< 0 dd)
          [ny nm dd]
          (days-and-months ny nm dd))))))

(defn normalize-d  [x]
  (if (and (:year x) (:month x) (:day x))
    (let [[y m d] (days-and-months (:year x) (:month x) (:day x))]
      (assoc x :year y :month m :day d))
    x))

(defn normalize [t]
  (->> t
       normalize-ms
       normalize-s
       normalize-mi
       normalize-h
       normalize-m
       normalize-d
       (remove (comp zero? val))
       (into {})))

(def defaults-units  [[:year 0] [:month 1] [:day 1] [:hour 0] [:min 0] [:sec 0] [:ms 0]])

(defn- after? [t t']
  (loop [[[p s] & ps] defaults-units]
    (let [tp (get t p s) tp' (get t' p s)]
      (cond
        (> tp tp') true
        (= tp tp') (and (seq ps) (recur ps))
        :else false))))

(def ^{:private true} default-time {:year 0 :month 1 :day 1 :hour 0 :min 0 :sec 0 :ms 0})

(defn- init-plus [r i]
  (->>
   (concat (keys (dissoc r :tz)) (keys i))
   (into #{})
   (reduce (fn [acc k] (assoc acc k (+ (get r k 0) (get i k 0)))) {})))

(defn plus
  ([] default-time)
  ([x] x)
  ([x y] (normalize (init-plus x y)))
  ([x y & more]
   (reduce plus (plus x y) more)))

(defn eq? [& ts]
  (apply = (map normalize ts)))

(def not-eq? (complement eq?))

(defn gt
  ([_] true)
  ([x y] (after? x y))
  ([x y & more]
   (if (gt x y)
     (if (next more)
       (recur y (first more) (next more))
       (gt y (first more)))
     false)))

(defn gte
  ([_] true)
  ([x y] (or (gt x y) (eq? x y)))
  ([x y & more]
   (if (gte x y)
     (if (next more)
       (recur y (first more) (next more))
       (gte y (first more)))
     false)))

(defn lt
  ([_] true)
  ([x & args] (apply (complement gte) x args)))

(defn lte
  ([_] true)
  ([x & args] (apply (complement gt) x args)))

(defn invert [x]
  (reduce
   (fn [x k] (cond-> x
              (contains? x k)
              (update k -)))
   x
   [:year :month :day :hour :min :sec :ms]))

(defn minus
  ([] default-time)
  ([x] x)
  ([x y] (normalize (init-plus x (invert y))))
  ([x y & more]
   (reduce minus (minus x y) more)))
