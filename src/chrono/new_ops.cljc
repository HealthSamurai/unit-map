(ns chrono.new-ops
  (:require [chrono.util :as u]))

;;;;;;;; range & sequence ;;;;;;;;
(defn process-range [pprev prev next nnext]
  {:pre [(and (not-every? nil? [pprev nnext])
              (every? some? [prev next]))]}
  {:start (or pprev prev)
   :step  (if (nil? pprev) (- nnext next) (- prev pprev))
   :end   (or nnext next)})

;; TODO: wrapping single enums into {:start :val :end :val :step 1} will help
;; to not call map? each time
;; but it may make harder to do fast arithmetics optimizations
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

;; TODO: do this when defining the type instead of on each call
(def process-sequence (memoize process-sequence*))

(defn concretize-range [rng value]
  (u/map-v #(u/try-call % value) rng))

;;;;;;;; contains & length & index ;;;;;;;;
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

(defn index-in-range
  "Returns negative index if range start is infinite, 0 index will be end of range."
  [rng value x]
  (let [{:keys [start step end]} (concretize-range rng value)]
    (when (range-contains-some rng value x)
      (if (u/infinite? start)
        (quot (- end x) step)
        (quot (- x start) step)))))

(defn range-length [rng value]
  (let [{:keys [start step end]} (concretize-range rng value)]
    (if (some u/infinite? [start end])
      ##Inf
      (-> (- end start) (quot step) inc))))

(defn sequence-length [s value]
  (->> (process-sequence s)
       (map #(if (map? %) (range-length % value) 1))
       (reduce + 0)))

(defn sequence-first-index [s value]
  (let [e (first (process-sequence s))
        r (when (map? e) (concretize-range e value))]
    (if (or (not (map? e)) (u/finite? (:start r)))
      0
      ##-Inf)))

(defn sequence-last-index [s value] ;; TODO: check for empty sequence?
  (let [e (last (process-sequence s))
        r (when (map? e) (concretize-range e value))]
    (if (or (not (map? e)) (u/finite? (:end r)))
      (dec (sequence-length s value))
      ##Inf)))

(defn index-in-sequence [s value x] ;; TODO: ##Inf & ##-Inf as x give an exception
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

;;;;;;;; type ;;;;;;;;
(declare get-type)

(defn delta-type? [value]
  (= 2 (count (get-type value))))

(defn value-type? [value]
  (= 1 (count (get-type value))))

(defn dispatch-definition [v]
  (if (value-type? v)
    (first (get-type v))
    :default))

;; This whole type dispatching system implementation looks ugly
(defmulti definition #'dispatch-definition)

(defn get-type [x]
  (let [[v d] (first (meta x))] ;; TODO: maybe use namespaced-keywords instead?
    (cond
      (nil? v)     [:default-type]
      (keyword? d) [v d]
      :else        (or (some-> (methods definition) (find v) key vector)
                       [:default-type v]))))

;; TODO: preserve fn start & end?
(defn to-delta-definition [s value]
  (let [first-idx (sequence-first-index s value)
        last-idx  (sequence-last-index s value)]
    (cond
      (every? u/infinite? [first-idx last-idx])
      [##-Inf '.. -2 -1 0 1 '.. ##Inf]

      (u/infinite? last-idx)
      [first-idx  (inc first-idx) '.. ##Inf]

      (u/infinite? first-idx)
      [##-Inf '.. (dec last-idx) last-idx]

      :else
      [first-idx (inc first-idx) '.. last-idx])))

(defmethod definition :default [value]
  (let [t              (get-type value)
        is-delta-type? (= 2 (count t))]
    (reduce-kv (if is-delta-type?
                 (fn [acc k v] (assoc acc k (to-delta-definition v value)))
                 (fn [acc k _] (assoc acc k [##-Inf '.. -2 -1 0 1 2 '.. ##Inf])))
               {}
               ((get-method definition (first t)) value))))

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

;;;;;;;; inc & dec ;;;;;;;;
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

;;;;;;;; delta ;;;;;;;;
(defn strip-zeros [delta]
  (reduce-kv
   (fn [acc k v] (cond-> acc (zero? v) (dissoc k)))
   delta
   delta))

(defn invert [x] ;; TODO: map only over type's values, not all keys
  {:pre [(delta-type? x)]}
  (u/map-v - x))

(defn assoc-delta [value delta]
  (assoc value (second (get-type delta)) delta))

(defn apply-delta [value delta]
  (-> (reduce (fn [acc [k _]] (add-to-unit k acc (get delta k 0)))
              value
              (reverse (definition delta)))
      (assoc-delta delta)))

(defn get-applied-deltas [value]
  (->> value
       (remove (comp (partial contains? (-> value definition keys set)) key))
       (map (fn [[k v]] (with-meta v {(first (get-type value)) k})))))

(defn remove-deltas [value]
  (->> (get-applied-deltas value)
       (map invert)
       (reduce (fn [v d]
                 (let [delta-key (second (get-type d))]
                   (-> v
                       (dissoc delta-key)
                       (apply-delta d)
                       (dissoc delta-key))))
               value)))

(defn drop-deltas [value]
  (->> (get-applied-deltas value)
       (map (comp second get-type))
       (apply dissoc value)))

(defn assoc-deltas [value deltas]
  (reduce assoc-delta value deltas))

(defn apply-deltas [value deltas]
  (reduce apply-delta value deltas))

(defn to-deltas [value new-deltas]
  (let [has-deltas? (seq (get-applied-deltas value))
        new-deltas? (seq new-deltas)]
    (cond
      (and has-deltas? new-deltas?) (-> (remove-deltas value) (apply-deltas new-deltas))
      new-deltas?                   (assoc-deltas value new-deltas)
      has-deltas?                   (drop-deltas value)
      :else                         value)))

(defn value->delta [value & [delta-meta]]
  (->> (definition value)
       (reduce-kv
        (fn [acc k s]
          (if-let [v (get value k)]
            (assoc acc k (index-in-sequence s value v))
            acc))
        (with-meta {} (make-delta-type (meta value) delta-meta)))
       strip-zeros))

(defn try-strip-zeros [x]
  (cond-> x (delta-type? x) strip-zeros))

(defn process-binary-op-args-deltas
  "If args are deltas, then zeros are stripped,
   otherwhise x's delta applied to y"
  [x y]
  {:pre [(= (get-type x) (get-type y))]}
  [(try-strip-zeros x)
   (-> (try-strip-zeros y)
       (to-deltas (get-applied-deltas x)))])

;;;;;;;; cmp ;;;;;;;;
(defn sequence-cmp [s value x y]
  (cond
    (= x y) 0
    (nil? x) -1
    (nil? y) 1
    (= x (sequence-contains-some s value x y)) -1
    :else 1))

(defn cmp [x y]
  (let [[x' y'] (process-binary-op-args-deltas x y)]
    (or (->> (definition x')
             reverse
             (map (fn [[unit sequence]] (sequence-cmp sequence x' (get x' unit) (get y' unit))))
             (drop-while zero?)
             first)
        0)))

(defn eq?
  ([_]          true)
  ([x y]        (zero? (cmp x y)))
  ([x y & more] (apply u/apply-binary-pred eq? x y more)))

(def not-eq? (complement eq?))

(defn lt?
  ([_]          true)
  ([x y]        (neg? (cmp x y)))
  ([x y & more] (apply u/apply-binary-pred lt? x y more)))

(defn gt?
  ([_]          true)
  ([x y]        (pos? (cmp x y)))
  ([x y & more] (apply u/apply-binary-pred gt? x y more)))

(defn lte?
  ([_]          true)
  ([x y]        (>= 0 (cmp x y)))
  ([x y & more] (apply u/apply-binary-pred lte? x y more)))

(defn gte?
  ([_]          true)
  ([x y]        (<= 0 (cmp x y)))
  ([x y & more] (apply u/apply-binary-pred gte? x y more)))

;;;;;;;; arithmetic ;;;;;;;;
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

(defn substract-delta [x delta]
  (plus x (invert delta)))

(defn difference
  "Difference between two values"
  [x y]
  (->> (cond-> (process-binary-op-args-deltas x y)
         (lt? x y) reverse)
       (map value->delta)
       (apply substract-delta)))

(defn minus
  "  a   -   a   = delta
   value - delta = value
   delta - value = error"
  ([x] (invert x))
  ([x y]
   {:pre [(or (value-type? x) (delta-type? y))]}
   (if (delta-type? y)
     (substract-delta x y)
     (cond-> (difference x y)
       (gt? y x) invert)))
  ([x y & more] (reduce minus (minus x y) more)))
