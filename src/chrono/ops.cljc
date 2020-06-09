(ns chrono.ops
  (:require [chrono.datetime :as cd]
            [chrono.interval :as ci]
            [chrono.util :as u]))


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

(def normalize-cd-ms (gen-norm ::cd/ms    ::cd/sec  1000 0))
(def normalize-cd-s  (gen-norm ::cd/sec   ::cd/min  60   0))
(def normalize-cd-mi (gen-norm ::cd/min   ::cd/hour 60   0))
(def normalize-cd-h  (gen-norm ::cd/hour  ::cd/day  24   0))
(def normalize-cd-m  (gen-norm ::cd/month ::cd/year 12   1))

(def normalize-ci-ms (gen-norm ::ci/ms    ::ci/sec  1000 0))
(def normalize-ci-s  (gen-norm ::ci/sec   ::ci/min  60   0))
(def normalize-ci-mi (gen-norm ::ci/min   ::ci/hour 60   0))
(def normalize-ci-h  (gen-norm ::ci/hour  ::ci/day  24   0))

(defn days-and-months [y m d]
  (if (<= 1 d 27)
    [y m d]
    (cond
      (> d 0)
      (let [num-days (u/days-in-month #::cd{:year y, :month m})
            dd (- d num-days)]
        (if (<= d num-days)
          [y m d]
          (if (= m 12)
            (days-and-months (inc y) 1 dd)
            (days-and-months y (inc m) dd))))

      (<= d 0)
      (let [[num-days ny nm] (if (= m 1)
                               [(u/days-in-month #::cd{:year (dec y), :month 12}) (dec y) 12]
                               [(u/days-in-month #::cd{:year y, :month (dec m)}) y (dec m)])
            dd (+ num-days d)]
        (if (< 0 dd)
          [ny nm dd]
          (days-and-months ny nm dd))))))

(defn normalize-cd-d [{::cd/keys [year month day] :as x}]
  (if (and year month day)
    (let [[y m d] (days-and-months year month day)]
      (assoc x ::cd/year y, ::cd/month m, ::cd/day d))
    x))

(defmulti normalize-rule (fn [unit _] unit))
(defmethod normalize-rule :default   [_ t] t)

(defmethod normalize-rule ::cd/ms    [_ t] (normalize-cd-ms t))
(defmethod normalize-rule ::cd/sec   [_ t] (normalize-cd-s t))
(defmethod normalize-rule ::cd/min   [_ t] (normalize-cd-mi t))
(defmethod normalize-rule ::cd/hour  [_ t] (normalize-cd-h t))
(defmethod normalize-rule ::cd/day   [_ t] (normalize-cd-d t))
(defmethod normalize-rule ::cd/month [_ t] (normalize-cd-m t))

(defmethod normalize-rule ::ci/ms    [_ t] (normalize-ci-ms t))
(defmethod normalize-rule ::ci/sec   [_ t] (normalize-ci-s t))
(defmethod normalize-rule ::ci/min   [_ t] (normalize-ci-mi t))
(defmethod normalize-rule ::ci/hour  [_ t] (normalize-ci-h t))

(def datetime-unit-defaults [[::cd/year 1]
                             [::cd/month 1]
                             [::cd/day 1]
                             [::cd/hour 0]
                             [::cd/min 0]
                             [::cd/sec 0]
                             [::cd/ms 0]])

(def interval-unit-defaults [[::ci/day 0]
                             [::ci/hour 0]
                             [::ci/min 0]
                             [::ci/sec 0]
                             [::ci/ms 0]])

(def unit-defaults
  (concat datetime-unit-defaults
          interval-unit-defaults))

(defn custom-units [t]
  (let [units-to-ignore (into #{} (conj (map first unit-defaults) ::cd/tz))
        current-units (into #{} (keys t))]
    (into [] (remove units-to-ignore current-units))))

(defn ordered-rules [t]
  (let [init [::cd/ms ::cd/sec ::cd/min ::cd/hour ::cd/month
              ::ci/ms ::ci/sec ::ci/min ::ci/hour]
        with-custom (apply conj (custom-units t) init)]
    (conj with-custom ::cd/day)))

(declare to-utc)
(declare to-tz)

(defn normalize [{::cd/keys [tz] :as t}]
  (let [rules           (ordered-rules t)
        normalized-time (reduce (fn [t unit] (normalize-rule unit t)) t rules)]
    (into {}
          (remove (every-pred (comp not #{::cd/tz} key) (comp zero? val)))
          normalized-time)))

(def ^:private default-time {:year 0 :month 1 :day 1 :hour 0 :min 0 :sec 0 :ms 0})

(defn- append-prefix-to-keys [m prefix]
  (reduce-kv (fn [r k v]
               (assoc r (keyword (name prefix) (name k)) v))
             {}
             m))

(defn- init-plus [a b]
  (let [{a-ch :chrono.datetime a-ci :chrono.interval} (u/group-keys a)
        {b-ch :chrono.datetime b-ci :chrono.interval} (u/group-keys b)
        tz (or (:tz a-ch) (:tz b-ch))
        result-namespace (if (or a-ch b-ch)
                           "chrono.datetime"
                           "chrono.interval")]
    (if (and a-ch b-ch)
      (throw (ex-info "Can't add 2 dates." {:operation `+, :params [a-ch b-ch]}))
      (cond-> (merge-with +
                          (or a-ch a-ci)
                          (or b-ch b-ci))
        :always (-> (append-prefix-to-keys result-namespace)
                    (dissoc ::cd/tz))
        (and (some? tz)
             (or a-ch b-ch)) (assoc ::cd/tz tz)))))

(defn plus
  ([]           default-time)
  ([x]          x)
  ([x y]        (normalize (init-plus x y)))
  ([x y & more] (reduce plus (plus x y) more)))

(defn invert [x]
  (reduce
   (fn [x k] (cond-> x
               (contains? x k)
               (update k -)))
   x
   [:year :month :day :hour :min :sec :ms]))

(defn- init-minus [a b]
  (let [{a-ch :chrono.datetime a-ci :chrono.interval} (u/group-keys a)
        {b-ch :chrono.datetime b-ci :chrono.interval} (u/group-keys b)
        tz (:tz a-ch)
        b-ch (when (some? b-ch)
               (-> (to-tz b tz)
                   u/group-keys
                   :chrono.datetime))
        result-namespace (if (or (and a-ch b-ch)
                                 (and a-ci b-ci))
                           "chrono.interval"
                           "chrono.datetime")]
    (if (and a-ci b-ch)
      (throw (ex-info "Can't subtract a date from an interval." {:operation `-, :params [a-ci b-ch]}))
      (cond-> (merge-with +
                          (or a-ch a-ci)
                          (invert (or b-ch b-ci)))
        true (dissoc :tz)
        true (append-prefix-to-keys result-namespace)
        (and (some? tz)
             (and a-ch b-ci)) (assoc ::cd/tz tz)))))

(defn minus
  ([]           default-time)
  ([x]          x)
  ([x y]        (normalize (init-minus x y)))
  ([x y & more] (reduce minus (minus x y) more)))

(def to-normalized-utc (comp normalize #(to-tz % 0)))

(defn- after? [t t']
  (loop [[[p s] & ps] unit-defaults]
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

(defmulti day-saving "[tz y]" (fn [tz _] tz))

(defn *day-saving-with-utc [tz y]
  (let [ds (day-saving tz y)]
    (assoc ds
           :in-utc (plus (:in ds) {::ci/hour (:offset ds)})
           :out-utc (plus (:out ds) {::ci/hour (+ (:offset ds) (:ds ds))}))))

(def day-saving-with-utc (memoize *day-saving-with-utc))

(defn kw-tz->utc0 [t] ;; TODO: make this work for any utc offset
  (let [ds (day-saving-with-utc (::cd/tz t) (::cd/year t))
        off (if (or (denormalised-lte t (:in ds)) (denormalised-gt t (:out ds)))
              (:offset ds)
              (+ (:offset ds) (:ds ds)))]
    (-> (dissoc t ::cd/tz)
        (plus {::ci/hour off}))))

(defn utc0->kw-tz [t tz] ;; TODO: make this work for any utc offset
  (let [ds (day-saving-with-utc tz (::cd/year t))
        off (if (or (denormalised-lte t (:in-utc ds)) (denormalised-gt t (:out-utc ds)))
              (:offset ds)
              (+ (:offset ds) (:ds ds)))]
    (-> (plus t {::ci/hour (- off)})
        (assoc ::cd/tz tz))))

(defn to-tz [{::cd/keys [tz] :as t} dtz]
  (cond
    (nil? dtz) (dissoc t ::cd/tz)
    (nil? tz)  (assoc t ::cd/tz dtz)
    :else
    (let [d (- (if (keyword? dtz) 0 dtz)
               (if (keyword? tz)  0 tz))]
      (cond-> t
        (keyword? tz)   kw-tz->utc0
        :always         (dissoc ::cd/tz)
        (not (zero? d)) (plus {::ci/hour d})
        (keyword? dtz)  (utc0->kw-tz dtz)
        :always         (assoc ::cd/tz dtz)))))

(defn to-utc [t] (to-tz t 0))
