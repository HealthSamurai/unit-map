(ns chrono.new-ops
  (:refer-clojure :exclude [type])
  (:require [chrono.util :as u]))

(defmulti type (comp ffirst meta)) ;; TODO: maybe use namespaced-keywords instead?

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

(defmulti range-contains?
  (fn [{:keys [start step end]} value x]
    (if (some fn? [start step end])
      :fn
      :const)))

(defmethod range-contains? :const [{:keys [start step end]} _ x]
  (and (<= start x end)
       (or (= start x)
           (= end x)
           (and (not= ##-Inf start)
                (-> x (- start) (mod step) zero?))
           (and (not= ##Inf end)
                (-> x (+ end) (mod step) zero?)))))

(defmethod range-contains? :fn [rng value x]
  (-> (reduce-kv (fn [acc k v] (assoc acc k (u/try-call v value))) {} rng)
      (range-contains? nil x)))

(defn sequence-contains? [s value x]
  (->> (process-sequence s)
       (some #(or (= x %)
                  (when (map? %) (range-contains? % value x))))
       boolean))

(defn get-next-unit-value [s value x]
  (loop [[el next & rest] (process-sequence s)]
    (cond
      (nil? el)
      nil

      (or (= x el)
          (and (map? el) (= x (:end el))))
      (cond-> next (map? next) (:start next))

      (and (map? el)
           (range-contains? el value x)
           (range-contains? el value (+ x (:step el))))
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
           (range-contains? el value x)
           (range-contains? el value (- x (:step el))))
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

(defn cmp [x y]
  {:pre [(= (type x) (type y))]} ;;TODO: maybe allow to compare across different types?
  (let [units       (reverse (keys (type x)))
        value-pairs (map (juxt (partial get x) (partial get y)) units)]
    (cond
      (every? (partial apply =) value-pairs) 0)))

(defn eq?
  ([x]          true)
  ([x y]        (= 0 (cmp x y)))
  ([x y & more] (apply u/apply-binary-pred eq? x y more)))

(def not-eq? (complement eq?))
