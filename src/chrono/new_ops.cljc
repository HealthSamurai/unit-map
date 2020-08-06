(ns chrono.new-ops
  (:require [chrono.util :as u]))

;; TODO: sequence and range may be types/records
;; Maybe this will help to get rid of
;; repeating checks for 'map?, process-range and concretize-range

(declare get-type)

(defmulti definition #'get-type)

(defn get-type [x]
  (let [[v d] (first (meta x))] ;; TODO: maybe use namespaced-keywords instead?
    (cond
      (nil? v)     :default-type
      (keyword? d) [v d]
      :else        (or (some-> (methods definition) (find v) key)
                       [:default-type v]))))

(defmethod definition :default [value] ;; TODO: take keys from deltas main type
  (reduce-kv (fn [acc k _] (assoc acc k [##-Inf '.. -2 -1 0 1 2 '.. ##Inf]))
             {}
             value))

(defn delta-type? [value]
  (let [t (get-type value)]
    (and (vector? t) (some? (second t)))))

(defn value-type? [value]
  (let [t (get-type value)]
    (or (keyword? t))))

(defn unit-type [value unit]
  (get (definition value) unit))

(defn make-delta-type [value-meta delta-type]
  (or (some-> (ffirst value-meta)
              (hash-map (or delta-type :delta)))
      (some-> delta-type (hash-map true))
      {:delta true}))

(defn get-next-unit [value unit]
  (u/get-next-element (keys (definition value)) unit))

(defn get-prev-unit [value unit]
  (u/get-prev-element (keys (definition value)) unit))

(defn process-range [pprev prev next nnext]
  {:pre [(and (not-every? nil? [pprev nnext])
              (every? some? [prev next]))]}
  {:start (or pprev prev)
   :step  (if (nil? pprev) (- nnext next) (- prev pprev))
   :end   (or nnext next)})

(defn process-sequence* [s]
  (loop [[pprev prev x next nnext & rest] (concat [nil nil] s [nil nil])
         result []
         buffer []]
    (cond
      (nil? x)  (concat result buffer)
      (= '.. x) (recur (concat [nil nil] rest)
                       (concat result
                               (drop-last 2 buffer)
                               [(process-range pprev prev next nnext)])
                       [])
      :else     (recur (concat [prev x next nnext] rest)
                       result
                       (conj buffer x)))))

(def process-sequence (memoize process-sequence*))

(defn concretize-range [rng value]
  (u/map-v #(u/try-call % value) rng))

(defn range-contains? [rng value x]
  (let [{:keys [start step end]} (concretize-range rng value)]
    (and (<= start x end)
         (or (= start x)
             (= end x)
             (and (not= ##-Inf start)
                  (-> x (- start) (mod step) zero?))
             (and (not= ##Inf end)
                  (-> x (+ end) (mod step) zero?))))))

(defn range-contains-some [rng value & xs]
  (->> (sort xs)
       (filter (partial range-contains? (concretize-range rng value) value))
       first))

(defn sequence-contains-some
  "Returns first (i.e. min) x found in the sequence"
  [s value x & xs]
  (let [xs (cons x xs)]
    (some (some-fn (set xs)
                   #(when (map? %) (apply range-contains-some % value xs)))
          (process-sequence s))))

(defn sequence-cmp [s value x y]
  (cond
    (= x y) 0
    (nil? x) -1
    (nil? y) 1
    (= x (sequence-contains-some s value x y)) -1
    :else 1))

(defn cmp [x y] ;;TODO: ignore zeros in delta types
  {:pre [(= (definition x) (definition y))]} ;;TODO: maybe allow to compare across different types?
  (or (->> (definition x)
           reverse
           (map (fn [[unit sequence]] (sequence-cmp sequence x (get x unit) (get y unit))))
           (filter (comp not zero?))
           first)
      0))
;; TODO: some checks will be faster without cmp
(defn eq?
  ([x]          true)
  ([x y]        (zero? (cmp x y)))
  ([x y & more] (apply u/apply-binary-pred eq? x y more)))

(def not-eq? (complement eq?))

(defn lt?
  ([x]          true)
  ([x y]        (neg? (cmp x y)))
  ([x y & more] (apply u/apply-binary-pred lt? x y more)))

(defn gt?
  ([x]          true)
  ([x y]        (pos? (cmp x y)))
  ([x y & more] (apply u/apply-binary-pred gt? x y more)))

(defn lte?
  ([x]          true)
  ([x y]        (>= 0 (cmp x y)))
  ([x y & more] (apply u/apply-binary-pred lte? x y more)))

(defn gte?
  ([x]          true)
  ([x y]        (<= 0 (cmp x y)))
  ([x y & more] (apply u/apply-binary-pred gte? x y more)))

(defn get-next-unit-value [s value x]
  (loop [[el next & rest] (process-sequence s)]
    (let [{:keys [step end]} (if (map? el) (concretize-range el value) {})]
      (cond
        (nil? el)
        nil

        (or (= x el) (= x end))
        (cond-> next (map? next) (-> :start (u/try-call value)))

        (and (map? el)
             (range-contains-some el value x)
             (range-contains-some el value (+ x step)))
        (+ x step)

        :else
        (recur (cons next rest))))))

(defn get-prev-unit-value [s value x]
  (loop [[prev el & rest] (cons nil (process-sequence s))]
    (let [{:keys [start step]} (if (map? el) (concretize-range el value) {})]
      (cond
        (nil? el)
        nil

        (or (= x el) (= x start))
        (cond-> prev (map? prev) (-> :end (u/try-call value)))

        (and (map? el)
             (range-contains-some el value x)
             (range-contains-some el value (- x step)))
        (- x step)

        :else
        (recur (cons el rest))))))

(defn get-min-value [value unit]
  (let [start (first (process-sequence (unit-type value unit)))]
    (if (map? start)
      (u/try-call (:start start) value)
      start)))

(defn get-max-value [value unit]
  (let [end (last (process-sequence (unit-type value unit)))]
    (if (map? end)
      (u/try-call (:end end) value)
      end)))

(defn inc-unit [unit {unit-value unit, :or {unit-value (get-min-value value unit)}, :as value}]
  (or (some->> unit-value
               (get-next-unit-value (unit-type value unit) value)
               (assoc value unit))
      (inc-unit (get-next-unit value unit)
                (assoc value unit (get-min-value value unit)))))

(defn dec-unit [unit {unit-value unit, :as value}]
  (or (some->> (if (some? unit-value)
                 (get-prev-unit-value (unit-type value unit) value unit-value)
                 (get-max-value value unit))
               (assoc value unit))
      (as-> value $
        (dissoc $ unit)
        (dec-unit (get-next-unit $ unit) $)
        (assoc $ unit (get-max-value $ unit)))))

(defn add-to-unit [unit value x]
  (let [f     (if (neg? x) dec-unit inc-unit)
        abs-x (if (neg? x) (- x) x)]
    (u/n-times abs-x (partial f unit) value)))

(defn substract-from-unit [unit value x]
  (add-to-unit unit value (- x)))

(defn strip-zeros [delta]
  (reduce-kv
   (fn [acc k v] (cond-> acc (zero? v) (dissoc k)))
   delta
   delta))

(defn plus
  "  a   + delta =   a
   delta +   a   =   a
   value + value = error"
  ([] {})
  ([x] x)
  ([x y] {:pre [(some delta-type? [x y])]}
   (let [[a b]  (if (delta-type? y) [x y] [y x])
         result (->> (reverse (definition b))
                     (reduce (fn [a' [k _]] (add-to-unit k a' (get b k 0)))
                             a))]
     (cond-> result (delta-type? result) strip-zeros)))
  ([x y & more] (reduce plus (plus x y) more)))

(defn index-in-range
  "Returns negative index if range start is infinite, 0 index will be end of range."
  [rng value x]
  (let [{:keys [start step end]} (concretize-range rng value)]
    (when (range-contains-some rng value x)
      (if (u/infinite? start)
        (- (quot (- x end) step))
        (quot (- x start) step)))))

(defn range-length [rng value]
  (let [{:keys [start step end]} (concretize-range rng value)]
    (if (some u/infinite? [start end])
      ##Inf
      (-> (- end start) (quot step) inc))))

(defn index-in-sequence [s value x]
  (loop [i 0, [el & rest-s] (process-sequence s)]
    (when (some? el)
      (let [increment (if (and (map? el) (u/finite? (:start el)))
                        (range-length el value)
                        1)

            index (cond
                    (= x el)  0
                    (map? el) (index-in-range el value x))]
        (if (some? index)
          (+ i index)
          (recur (+ i increment) rest-s))))))

(defn value->delta [value & [delta-meta]]
  (reduce-kv
   (fn [acc k v]
     (let [i (index-in-sequence (unit-type value k) value v)]
       (cond-> acc (not (zero? i)) (assoc k i))))
   (with-meta {} (make-delta-type (meta value) delta-meta))
   value))

(defn invert [x] ;; TODO: map only over type's values, not all keys
  {:pre [(delta-type? x)]}
  (u/map-v - x))

(declare difference)

(defn minus
  "  a   -   a   = delta
   value - delta = value
   delta - value = error"
  ([x] (invert x))
  ([x y]
   {:pre [(or (value-type? x) (delta-type? y))]}
   (if (delta-type? y)
     (plus x (invert y))
     (cond-> (difference x y)
       (gt? y x) invert)))
  ([x y & more] (reduce minus (minus x y) more)))

(defn difference
  "Difference between two values"
  [x y]
  (->> (if (gte? x y) [x y] [y x])
       (map value->delta)
       (apply minus)))

(defn apply-delta [value delta]
  (assoc (plus value delta)
         (second (get-type delta)) delta))

(defn get-applied-deltas [value]
  (->> value
       (remove (comp (partial contains? (-> value definition keys set)) key))
       (map (fn [[k v]] (with-meta v {(get-type value) k})))))
