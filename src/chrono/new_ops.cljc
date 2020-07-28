(ns chrono.new-ops
  (:refer-clojure :exclude [type])
  (:require [chrono.util :as u]))

;; TODO: sequence and range may be types/records
;; Maybe this will help to get rid of
;; repeating checks for 'map?, process-range and concretize-range

(def get-type (some-fn (comp ffirst meta) (constantly :default-type)))

(defmulti type get-type) ;; TODO: maybe use namespaced-keywords instead?

(defmethod type :default [value]
  (reduce-kv (fn [acc k _] (assoc acc k [##-Inf '.. -2 -1 0 1 2 '.. ##Inf]))
             {}
             value))

(defn delta-type? [value]
  (and (some? (get-type value))
       (not (contains? (methods type) (get-type value)))))

(defn value-type? [value]
  (or (nil? (get-type value))
      (contains? (methods type) (get-type value))))

(defn unit-type [value unit]
  (get (type value) unit))

(defn get-next-unit [value unit]
  (u/get-next-element (keys (type value)) unit))

(defn get-prev-unit [value unit]
  (u/get-prev-element (keys (type value)) unit))

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

(defn range-contains-some [rng value & xs]
  (let [{:keys [start step end]} (concretize-range rng value)]
    (->> (sort xs)
         (filter #(and (<= start % end)
                       (or (= start %)
                           (= end %)
                           (and (not= ##-Inf start)
                                (-> % (- start) (mod step) zero?))
                           (and (not= ##Inf end)
                                (-> % (+ end) (mod step) zero?)))))
         first)))

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
  {:pre [(= (type x) (type y))]} ;;TODO: maybe allow to compare across different types?
  (or (->> (type x)
           reverse
           (map (fn [[unit sequence]] (sequence-cmp sequence x (get x unit) (get y unit))))
           (filter (comp not zero?))
           first)
      0))

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

(defn plus
  "  a   + delta =   a
   delta +   a   =   a
   value + value = error"
  ([] {})
  ([x] x)
  ([x y] {:pre [(some delta-type? [x y])]}
   (let [[a b] (if (delta-type? y) [x y] [y x])]
     (with-meta (->> (type a)
                     reverse
                     (reduce (fn [a' [k _]] (add-to-unit k a' (get b k 0))) a)
                     (merge b))
       (meta a))))
  ([x y & more] (reduce plus (plus x y) more)))

(defn index-in-range [rng value x]
  (let [{:keys [start step]} (concretize-range rng value)]
    (if (u/infinite? start)
      ##Inf ;; TODO: negative indexing for ranges starting with infinity
      (quot (- x start) step))))

(defn range-length [rng value]
  (let [{:keys [start step end]} (concretize-range rng value)]
    (if (some u/infinite? [start end])
      ##Inf
      (inc (quot (- end start) step)))))

(defn index-in-sequence [s value x]
  (loop [i 0
         [el & rest-s] (process-sequence s)]
    (when (some? el)
      (let [[increment index]
            (if (map? el)
              [(range-length el value) (index-in-range el value x)]
              [1 (when (= x el) 0)])]
        (if (some? index)
          (+ i index)
          (recur (+ i increment) rest-s))))))

(defn value->delta [value & [meta]]
  (with-meta
    (u/map-kv (fn [k v]
                (let [i (index-in-sequence (unit-type value k) value v)]
                  (if (u/infinite? i) v i))) ;; TODO: returning v here is wrong
              value)                         ;; for example: 1a.d. - 1b.c. will be 2, should be 1
    (or meta {:delta true})))

(defn invert [x]
  {:pre [(delta-type? x)]}
  (u/map-v - x))

(declare difference)

(defn minus
  "  a   -   a   = delta
   value - delta = value
   delta - value = error"
  ([x] (invert x))
  ([x y]
   {:pre [(not (and (delta-type? x) (value-type? y)))]}
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
