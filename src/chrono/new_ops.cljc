(ns chrono.new-ops
  (:require [chrono.util :as u]))

(defmulti rules (comp ffirst meta))

(defmethod rules :default [_]
  (array-map
   :ms     [0 1 '.. 999]
   :sec    [0 1 '.. 59]
   :min    [0 1 '.. 59]
   :hour   [0 1 '.. 23]
   :day    [1 2 '.. u/days-in-month]
   :month  [:jan :feb :mar :apr :may :jun :jul :aug :sep :oct :nov :dec]
   :year   [##-Inf '.. -2 -1 1 2 '.. ##Inf]))

(defmethod rules :am-pm [_]
  (array-map
   :ms     [0 1 '.. 999]
   :sec    [0 1 '.. 59]
   :min    [0 1 '.. 59]
   :hour   [12 1 2 '.. 11]
   :period [:am :pm]
   :day    [1 2 '.. u/days-in-month]
   :month  [:jan :feb :mar :apr :may :jun :jul :aug :sep :oct :nov :dec]
   :year   [##-Inf '.. -2 -1 1 2 '.. ##Inf]))

(defn get-rules [value unit]
  (get (rules value) unit))

(defn get-next-unit [value unit]
  (u/get-next-element (keys (rules value)) unit))

(defn get-prev-unit [value unit]
  (u/get-prev-element (keys (rules value)) unit))

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
  (let [start (first (process-sequence (get-rules value unit)))]
    (if (map? start)
      (u/try-call (:start start) value)
      start)))

(defn get-max-value [value unit]
  (let [end (last (process-sequence (get-rules value unit)))]
    (if (map? end)
      (u/try-call (:end end) value)
      end)))

(defn inc-unit [unit value]
  (or (some->> (or (get value unit)
                   (get-min-value value unit))
               (get-next-unit-value (get-rules value unit) value)
               (assoc value unit))
      (inc-unit (get-next-unit value unit)
                (assoc value unit (get-min-value value unit)))))

(defn dec-unit [unit value]
  (or (some->> (if-let [unit-value (get value unit)]
                 (get-prev-unit-value (get-rules value unit) value unit-value)
                 (get-max-value value unit))
               (assoc value unit))
      (dec-unit (get-next-unit value unit)
                (assoc value unit (get-max-value value unit)))))
