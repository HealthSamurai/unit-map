(ns chrono.new-ops
  (:refer-clojure :exclude [type])
  (:require [chrono.util :as u]))

(def get-type (some-fn (comp ffirst meta) (constantly :default-type)))

(defmulti type get-type) ;; TODO: maybe use namespaced-keywords instead?

(defmethod type :default [value]
  (reduce-kv (fn [acc k _] (assoc acc k [##-Inf '.. -2 -1 0 1 2 '.. ##Inf]))
             {}
             value))

(defn delta-type? [value]
  (and (some? (get-type value))
       (not (contains? (methods type) (get-type value)))))

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
      (= '.. x) (recur rest
                       (concat result
                               (drop-last 2 buffer)
                               [(process-range pprev prev next nnext)])
                       [])
      :else     (recur (concat [prev x next nnext] rest)
                       result
                       (conj buffer x)))))

(def process-sequence (memoize process-sequence*))

(defn range-contains-some [rng value & xs]
  (let [{:keys [start step end]} (u/map-values #(u/try-call % value) rng)]
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

(defn get-next-unit-value [s value x]
  (loop [[el next & rest] (process-sequence s)]
    (cond
      (nil? el)
      nil

      (or (= x el)
          (and (map? el) (= x (:end el))))
      (cond-> next (map? next) (:start next))

      (and (map? el)
           (range-contains-some el value x)
           (range-contains-some el value (+ x (:step el))))
      (+ x (:step el))

      :else
      (recur (cons next rest)))))

(defn get-prev-unit-value [s value x]
  (loop [[prev el & rest] (cons nil (process-sequence s))]
    (cond
      (nil? el)
      nil

      (or (= x el)
          (and (map? el) (= x (:start el))))
      (cond-> prev (map? prev) (:end prev))

      (and (map? el)
           (range-contains-some el value x)
           (range-contains-some el value (- x (:step el))))
      (- x (:step el))

      :else
      (recur (cons el rest)))))

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
  ([x]          x)
  ([x y] {:pre [(some delta-type? [x y])]}
   (let [[a b] (if (delta-type? x) [y x] [x y])]
     (with-meta (->> (type a)
                     reverse
                     (reduce (fn [a' [k _]] (add-to-unit k a' (get b k 0))) a)
                     (merge b))
       (meta a))))
  ([x y & more] (reduce plus (plus x y) more)))

(defn sequence-cmp [s value x y]
  (cond
    (= x y) 0
    (nil? x) -1
    (nil? y) 1
    (= x (sequence-contains-some s value x y)) -1
    :else 1))

(defn cmp [x y]
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
