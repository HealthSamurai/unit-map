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

(defmulti normalize-rule (fn [unit _] unit))
(defmethod normalize-rule :default [_ t] t)
(defmethod normalize-rule :ms [_ t] (normalize-ms t))
(defmethod normalize-rule :sec [_ t] (normalize-s t))
(defmethod normalize-rule :min [_ t] (normalize-mi t))
(defmethod normalize-rule :hour [_ t] (normalize-h t))
(defmethod normalize-rule :day [_ t] (normalize-d t))
(defmethod normalize-rule :month [_ t] (normalize-m t))

(def defaults-units  [[:year 0] [:month 1] [:day 1] [:hour 0] [:min 0] [:sec 0] [:ms 0]])
(defn custom-units [t]
  (let [units-to-ignore (into #{} (conj (map first defaults-units) :tz))
        current-units (into #{} (keys t))]
    (into [] (remove units-to-ignore current-units))))

(defn ordered-rules [t]
  (let [init [:ms :sec :min :hour :month]
        with-custom (apply conj (custom-units t) init)]
    (conj with-custom :day)))

(defmulti to-utc (fn [{:keys [tz]}] (type tz)))

(defn normalize [t]
  (let [rules (ordered-rules t)
        normalized-time (reduce (fn [t unit] (normalize-rule unit t)) (to-utc t) rules)]
    (into {}
          (remove (comp zero? val))
          normalized-time)))

(def ^{:private true} default-time {:year 0 :month 1 :day 1 :hour 0 :min 0 :sec 0 :ms 0})

(defn- init-plus [r i]
  (let [r-utc (to-utc r)
        i-utc (to-utc i)]
    (->>
     (concat (keys r-utc) (keys i-utc))
     (into #{})
     (reduce (fn [acc k] (assoc acc k (+ (get r-utc k 0) (get i-utc k 0)))) {}))))

(defn plus
  ([] default-time)
  ([x] x)
  ([x y] (normalize (init-plus x y)))
  ([x y & more]
   (reduce plus (plus x y) more)))

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

(defmethod to-utc
  nil
  [t]
  t)

(defmethod to-utc
  Long
  [{:keys [hour tz] :as t}]
  (-> t
      (dissoc :tz)
      (minus {:hour tz})))

(defmulti to-tz "[t tz]" (fn [_ tz] (type tz)))

(defmethod to-tz
  nil
  [t & _]
  t)

(defmethod to-tz
  Long
  [t tz]
  (-> t
      (plus {:hour tz})
      (assoc :tz tz)))

(defn to-normalized-utc [t]
  (-> t
      to-utc
      normalize))

(defn- after? [t t']
  (loop [[[p s] & ps] defaults-units]
    (let [t->tp #(get % p s)
          tp (t->tp t)
          tp' (t->tp t')]
      (cond
        (> tp tp') true
        (= tp tp') (and (seq ps) (recur ps))
        :else false))))

(defn eq? [& ts]
  (apply = (map to-normalized-utc ts)))

(def not-eq? (complement eq?))

(defn gt
  ([_] true)
  ([x y] (after? (to-normalized-utc x) (to-normalized-utc y)))
  ([x y & more]
   (if (gt x y)
     (if (next more)
       (recur y (first more) (next more))
       (gt y (first more)))
     false)))

(defn denormalised-gt
  ([_] true)
  ([x y] (after? x y))
  ([x y & more]
   (if (denormalised-gt x y)
     (if (next more)
       (recur y (first more) (next more))
       (denormalised-gt y (first more)))
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

(defn denormalised-lte
  ([_] true)
  ([x & args] (apply (complement denormalised-gt) x args)))

(defn cmp [x y]
  (cond
    (eq? x y) 0
    (gt x y)  1
    (lt x y)  -1))

(defn from-utc [t tz])

(defn *more-or-eq [y m dw d]
  (let [dw' (u/day-of-week y m d)]
    (cond (= dw' dw) d
          ;; if wed vs sun
          (> dw' dw) (+ d (- 7 dw') dw)
          (< dw' dw) (+ d (- dw dw')))))

(def more-or-eq (memoize *more-or-eq))

(defmulti day-saving "[tz y]" (fn [tz _] tz))

(defmethod day-saving
  :ny
  [_ y]
  (assert (> y 2006) "Not impl.")
  {:offset 5
   :ds -1
   :in {:year y :month 3 :day (more-or-eq y 3 0 8) :hour 2 :min 0}
   :out {:year y :month 11 :day (more-or-eq y 11 0 1) :hour 2 :min 0}})

(defn *day-saving-with-utc [tz y]
  (let [ds (day-saving tz y)]
    (assoc ds
           :in-utc (plus (:in ds) {:hour (:offset ds)})
           :out-utc (plus (:out ds) {:hour (+ (:offset ds) (:ds ds))}))))

(def day-saving-with-utc (memoize *day-saving-with-utc))

(defmethod to-utc
  clojure.lang.Keyword
  [t]
  (let [ds (day-saving-with-utc (:tz t) (:year t))
        off (if (or (denormalised-lte t (:in ds)) (denormalised-gt t (:out ds)))
              (:offset ds)
              (+ (:offset ds) (:ds ds)))]
    (-> t
        (dissoc :tz)
        (plus {:hour off}))))

(defmethod to-tz
  clojure.lang.Keyword
  [t tz]
  (let [ds (day-saving-with-utc tz (:year t))
        off (if (or (denormalised-lte t (:in-utc ds)) (denormalised-gt t (:out-utc ds)))
              (:offset ds)
              (+ (:offset ds) (:ds ds)))]
    (-> t
        (plus {:hour (- off)})
        (assoc :tz tz))))

;; https://alcor.concordia.ca/~gpkatch/gdate-algorithm.html
;; https://alcor.concordia.ca/~gpkatch/gdate-method.html

(defn *ddd [y]
  (+ (* 365 y)
     (quot y 4)
     (quot y -100)
     (quot y 400)))

(defn *mmm [m]
  (->
   (* 306 m)
   (+ 5)
   (quot 10)))

(defn g [y m d]
  (let [m (rem (+ m 9) 12)
        y (- y (quot m 10))]
    (+ (*ddd y) (*mmm m) (dec d))))


(defn d [g]
  (let [y (-> (* 10000 g)
              (+ 14780)
              (quot 3652425))
        ddd (- g (*ddd y))
        y   (if (< ddd 0) (dec y) y)
        ddd (if (< ddd 0) (- g (*ddd y)) ddd)
        mi (-> (* 100 ddd)
               (+ 52)
               (quot 3060))
        mm (-> (+ mi 2)
               (rem 12)
               (+ 1))
        y (+ y (-> (+ mi 2) (quot 12)))
        dd  (+ ddd (- (*mmm mi)) 1)]
    [y mm dd]))

(comment
  (g 2018 3 1)
  (d (g 2018 1 1))
  (d (g 2018 3 5))
  (d (g 2018 7 2))
  (d (- (g 2018 1 1) 1))
  (d (+ (g 2017 12 31) 1))
  (d (+ (g 2017 12 31) 370)))
