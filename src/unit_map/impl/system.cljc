(ns unit-map.impl.system
  (:require [unit-map.util :as util]
            [clojure.set]))


;;;;;;;;;; sys info


(defn get-units [unit-map]
  (->> unit-map
       (mapcat (fn [[k v]]
                 (if (map? v)
                   (get-units v)
                   [k])))
       set))


(defn supporting-systems [all-systems units]
  (->> all-systems
       (filter (comp (partial clojure.set/subset? units)
                     set))
       sort))


(def a (atom #{}))

(def guess-sys*
  (memoize
    (fn [registry units]
      (doto (first (supporting-systems (vals (:systems registry)) units))
        (->> (map name) (swap! a conj ) )
        ))))


(defn guess-sys
  ([registry unit-map unit]
   (guess-sys registry (assoc unit-map unit nil)))
  ([registry unit-map]
   (when-let [units (not-empty (get-units unit-map))]
     (guess-sys* registry units))))


(defn sys-intersection [registry & unit-maps]
  (guess-sys registry (reduce merge unit-maps)))


(defn find-diff-branches [xs ys]
  (loop [cur-xs xs
         cur-ys ys
         result []]
    (if (and (empty? cur-xs) (empty? cur-ys))
      (not-empty result)
      (let [equal-pairs-len     (->> (map vector cur-xs cur-ys)
                                     (take-while (fn [[x y]] (= x y)))
                                     count)
            [equal-xys rest-xs] (split-at equal-pairs-len cur-xs)
            rest-ys             (drop equal-pairs-len cur-ys)

            [x-branch rest-xs'] (split-with (complement (set rest-ys)) rest-xs)
            branch-end          (first rest-xs')
            [y-branch rest-ys'] (if (some? branch-end)
                                  (split-with #(not= branch-end %) rest-ys)
                                  [rest-ys])]
        (recur rest-xs'
               rest-ys'
               (cond-> (into result equal-xys)
                 (or (seq x-branch) (seq y-branch))
                 (conj ^::branches[(vec x-branch) (vec y-branch)])))))))


(defn find-conversion [registry x y]
  (let [branches-diff (or (sys-intersection registry x y)
                          (find-diff-branches (guess-sys registry x) #_"TODO: find better sys match algo"
                                              (guess-sys registry y)))
        conv-start (first branches-diff)
        valid? (or (not (::branches (meta conv-start)))
                   (let [[[x :as xs] [y :as ys]] conv-start]
                     (or (empty? xs)
                         (empty? ys)
                         (contains? (->> (:eq-units registry)
                                         (filter #(contains? % x))
                                         first)
                                    y))))]
    (when valid?
      (mapv (fn [p]
              (if (::branches (meta p))
                {(first p) (second p)}
                {[p] [p]}))
            branches-diff))))


;;;;;;;;;; seq & range utils


(defn range? [x]
  (and (map? x)
       (every? #(contains? x %)
               [:start :end :step])))


(defn dynamic-useq? [useq]
  (boolean (some #(and (range? %)
                       (some fn? (vals %)))
                 useq)))


(defn static-useq? [useq]
  (not (dynamic-useq? useq)))


(defn concretize-range [rng umap]
  (update-vals rng #(util/try-call % umap)))


(defn range-length [rng umap]
  (let [{:keys [start step end]} (concretize-range rng umap)]
    (if (some util/infinite? [start end])
      ##Inf
      (-> (- end start) (quot step) inc))))


(defn useq-length [useq umap]
  (->> useq
       (map #(if (range? %) (range-length % umap) 1))
       (reduce + 0)))


(defn useq-first-index [useq umap]
  (let [e (first useq)
        r (when (range? e) (concretize-range e umap))]
    (cond
      (nil? e)                 nil
      (util/infinite? (:start r)) ##-Inf
      :else                    0)))


(defn useq-last-index [useq umap]
  (let [e (last useq)
        r (when (range? e) (concretize-range e umap))]
    (cond
      (nil? e)               nil
      (util/infinite? (:end r)) ##Inf
      :else                  (dec (useq-length useq umap)))))


(defn range-contains? [rng umap x]
  (let [{:keys [start step end]} (concretize-range rng umap)]
    (and (or (<= start x end)
             (>= start x end))
         (or (= start x)
             (= end x)
             (and (util/finite? start)
                  (-> x (- start) (mod step) zero?))
             (and (util/finite? end)
                  (-> x (+ end) (mod step) zero?))))))


(defn range-contains-some [rng umap & xs]
  (->> (sort xs)
       (filter (partial range-contains? rng umap))
       first))


(defn useq-contains-some
  "Returns first (i.e. min) x found in the s"
  [useq umap x & xs]
  (let [xs (cons x xs)]
    (some (some-fn (set xs)
                   #(when (range? %) (apply range-contains-some % umap xs)))
          useq)))


(defn range-index-of
  "Returns negative index if range start is infinite, 0 index will be end of range."
  [rng umap x]
  (let [{:as crng, :keys [start step end]} (concretize-range rng umap)]
    (cond
      (not (range-contains-some crng umap x))   nil
      (and (util/infinite? x) (util/infinite? start)) ##-Inf
      (util/infinite? x)                           ##Inf
      (util/infinite? start)                       (- (quot (- end x) step))
      :else                                     (quot (- x start) step))))


(defn useq-index-of [useq umap x]
  (loop [i 0, [el & rest-s] useq]
    (when (some? el)
      (or (some-> (cond
                    (= x el)    0
                    (range? el) (range-index-of el umap x))
                  (+ i))
          (recur (+ i (if (and (range? el) (util/finite? (:start el)))
                        (range-length el umap)
                        1))
                 rest-s)))))


(defn range-nth [rng umap index]
  (let [{:keys [start step end]} (concretize-range rng umap)]
    (if (util/infinite? start)
      (+ end (* step index))
      (+ start (* step index)))))


(defn useq-nth [useq umap index]
  (loop [i 0, [el & rest-s] useq]
    (when (some? el)
      (let [increment (if (and (range? el) (util/finite? (:start el)))
                        (range-length el umap)
                        1)

            result (cond
                     (not (or (<= i index (+ i increment -1))
                              (neg? index)))
                     nil

                     (range? el) (range-nth el umap (- index i))
                     :else       el)]
        (if (some? result)
          result
          (recur (+ i increment) rest-s))))))


;;;;;;;;;; system utils


(defn get-next-unit
  "next = more significant"
  [registry umap unit]
  (util/get-next-element (guess-sys registry umap unit) unit))


(defn get-prev-unit
  "prev = less significant"
  [registry umap unit]
  (util/get-prev-element (guess-sys registry umap unit) unit))


(defn useq [useqs unit next-unit]
  (get-in useqs [unit next-unit :useq]))


(def get-useq*
  (memoize
    (fn [registry unit next-unit]
      (useq (:seqs registry) unit next-unit))))


(defn get-useq [registry umap unit]
  (let [sys       (guess-sys registry umap unit)
        next-unit (util/get-next-element sys unit)]
    (get-useq* registry unit next-unit)))


(defn sys-useqs [registry sys]
  (map (fn [unit next-unit]
         [unit (get-useq* registry unit next-unit)])
       sys
       (rest (conj sys nil))))


;;;;;;;;;; next/prev /first/last min/max


(defn get-next-unit-value [useq umap x]
  (loop [[el next & rest] useq]
    (let [{:keys [step end]} (if (range? el)
                               (concretize-range el umap)
                               {})]
      (cond
        (nil? el)
        nil

        (or (= x el) (= x end))
        (cond-> next (range? next) (-> :start (util/try-call umap)))

        (and (range? el)
             (range-contains-some el umap x)
             (range-contains-some el umap (+ x step)))
        (+ x step)

        :else
        (recur (cons next rest))))))


(defn get-prev-unit-value [useq umap x]
  (loop [[prev el & rest] (cons nil useq)]
    (let [{:keys [start step]} (if (range? el) (concretize-range el umap) {})]
      (cond
        (nil? el)
        nil

        (or (= x el) (= x start))
        (cond-> prev (range? prev) (-> :end (util/try-call umap)))

        (and (range? el)
             (range-contains-some el umap x)
             (range-contains-some el umap (- x step)))
        (- x step)

        :else
        (recur (cons el rest))))))


(defn get-first-el [useq umap]
  (let [start (first useq)]
    (if (range? start)
      (util/try-call (:start start) umap)
      start)))


(defn get-last-el [useq umap]
  (let [end (last useq)]
    (if (range? end)
      (util/try-call (:end end) umap)
      end)))


(defn get-min-value [registry umap unit]
  (get-first-el (get-useq registry umap unit) umap))


(defn get-max-value [registry umap unit]
  (get-last-el (get-useq registry umap unit) umap))

