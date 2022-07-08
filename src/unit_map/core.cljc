(ns unit-map.core
  (:require [unit-map.util :as u]
            [clojure.set]
            [clojure.data]))


#_"TODO:
- guess-sys-with-seqs
- refactor (first (guess-sys ...))
- refactor repeating guess-sys calls
- use plural of unit for deltas (intervals)? e.g.: {:month :jul} and {:months 7}
- refactor to be able to pass ctx"


(defonce ctx #_"TODO: can it be done without global atom state?"
  (atom nil))
#_(reset! ctx nil)


;;;;;;;;;; read seq


(defn range? [x]
  (and (map? x) (::range (meta x))))


(defn process-range [pprev prev next-seq nnext]
  {:pre [(and (not-every? nil? [pprev nnext])
              (every? some? [prev next-seq]))]}
  ^::range{:start (or pprev prev)
           :step  (if (nil? pprev)
                    (if (integer? next-seq)
                      (- nnext next-seq)
                      next-seq)
                    (if (integer? prev)
                      (- prev pprev)
                      prev))
           :end   (or nnext next-seq)})


(defn process-equivalent [s]
  (if (= '<=> (second s))
    {:eq-unit (first s)
     :sequence (vec (drop 2 s))}
    {:sequence s}))


(defn process-next-unit [s]
  (if (= '-> (get s (- (count s) 2)))
    {:sequence (vec (drop-last 2 s))
     :next-unit (last s)}
    {:sequence s}))


(defn process-enumeration [s]
  {:sequence
   (loop [[pprev prev x next-seq nnext & rest] (concat [nil nil] s [nil nil])

          result []
          buffer []]
     (cond
       (nil? x)  (vec (concat result buffer))
       (= '.. x) (recur (concat [nil nil] rest)
                        (concat result
                                (drop-last 2 buffer)
                                [(process-range pprev prev next-seq nnext)])
                        [])
       :else     (recur (concat [prev x next-seq nnext] rest)
                        result
                        (conj buffer x))))})


(defn process-sequence* [s]
  (as-> s $
    (process-equivalent $)
    (merge $ (process-next-unit (:sequence $)))
    (merge $ (process-enumeration (:sequence $)))))


(def process-sequence (memoize process-sequence*))


(defn read-sequence [form]
  (process-sequence form))


;;;;;;;;;; defseq & defsys


(defn push-to-seq-graph [seqs-map unit unit-seq]
  (let [eq-seqs (when-let [eq-unit (:eq-unit unit-seq)]
                  (->> (vals seqs-map)
                       (keep #(get % eq-unit))))

        to-this-unit-new (->> eq-seqs
                              (map #(assoc % :next-unit unit))
                              distinct)

        to-this-unit (->> (vals seqs-map)
                          (keep #(get % unit)))

        to-eq-new (when-let [eq-unit (:eq-unit unit-seq)]
                    (->> to-this-unit
                         (map #(assoc % :next-unit eq-unit))
                         distinct))

        this-seq (assoc unit-seq :unit unit)

        to-save (concat [this-seq]
                        to-this-unit-new
                        to-eq-new)]
    (reduce (fn [acc s]
              (assoc-in acc
                        [(:unit s) (:next-unit s)]
                        (dissoc s :eq-unit)))
            seqs-map
            to-save)))


(defn push-to-eq-units [eq-units-sets unit {:keys [eq-unit]}]
  (let [group (->> eq-units-sets
                   (filter #(or (get % unit) (get % eq-unit)))
                   first)
        new-group (-> (or group #{})
                      (conj unit)
                      (cond-> (some? eq-unit) (conj eq-unit)))]
    (-> (or eq-units-sets #{})
        (disj group)
        (conj new-group))))


(defn defseq* [ctx unit unit-seq]
  (swap! ctx #(-> %
                  (update :seqs push-to-seq-graph unit unit-seq)
                  (update :eq-units push-to-eq-units unit unit-seq)))
  unit-seq)


(defmacro defseq [unit unit-seq]
  `(defseq* ctx ~unit ~unit-seq))


(defn sys-continuous?* [ctx units]
  (let [reverse-units (reverse units)]
    (->> (map vector
              (cons nil reverse-units)
              reverse-units)
         (every?
           (fn [[cur-unit prev-unit]]
             (get-in @ctx [:seqs prev-unit cur-unit]))))))


(defn sys-continuous? [units]
  (sys-continuous?* ctx units))


(defn defsys* [ctx sys-name units]
  (assert (sys-continuous?* ctx units))
  (swap! ctx assoc-in [:systems sys-name] units)
  units)


(defmacro defsys [sys-name units]
  `(defsys* ctx (quote ~sys-name) ~units))


;;;;;;;;;; sys info


(defn get-units [unit-map]
  (->> unit-map
       (mapcat (fn [[k v]]
                 (if (map? v)
                   (get-units v)
                   [k])))
       set))


(defn guess-ctx-sys [ctx units]
  (->> (vals (:systems ctx))
       (filter (comp (partial clojure.set/subset? units)
                     set))
       sort))


(def guess-sys*
  (memoize
    (fn [units]
      (guess-ctx-sys @ctx units))))


(defn guess-sys
  ([unit-map unit]
   (guess-sys (assoc unit-map unit nil)))
  ([unit-map]
   (when-let [units (not-empty (get-units unit-map))]
     (guess-sys* units))))


(defn sys-intersection [& unit-maps]
  (guess-sys (reduce merge unit-maps)))


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


(defn find-conversion [x y]
  (let [x-syss (guess-sys x)
        y-syss (guess-sys y)
        branches-diff (or (first (sys-intersection x y))
                          (find-diff-branches (first x-syss) #_"TODO: find better sys match algo"
                                              (first y-syss)))
        conv-start (first branches-diff)
        valid? (or (not (::branches (meta conv-start)))
                   (let [[[x :as xs] [y :as ys]] conv-start]
                     (or (empty? xs)
                         (empty? ys)
                         (contains? (->> (:eq-units @ctx)
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


(defn dynamic-sequence? [useq]
  (boolean (some #(and (range? %)
                       (some fn? (vals %)))
                 (:sequence useq))))


(defn static-sequence? [useq]
  (not (dynamic-sequence? useq)))


(defn concretize-range [rng umap]
  (update-vals rng #(u/try-call % umap)))


(defn range-length [rng umap]
  (let [{:keys [start step end]} (concretize-range rng umap)]
    (if (some u/infinite? [start end])
      ##Inf
      (-> (- end start) (quot step) inc))))


(defn sequence-length [useq umap]
  (->> (:sequence useq)
       (map #(if (range? %) (range-length % umap) 1))
       (reduce + 0)))


(defn sequence-first-index [useq umap]
  (let [e (first (:sequence useq))
        r (when (range? e) (concretize-range e umap))]
    (cond
      (nil? e)                 nil
      (u/infinite? (:start r)) ##-Inf
      :else                    0)))


(defn sequence-last-index [useq umap]
  (let [e (last (:sequence useq))
        r (when (range? e) (concretize-range e umap))]
    (cond
      (nil? e)               nil
      (u/infinite? (:end r)) ##Inf
      :else                  (dec (sequence-length useq umap)))))


(defn range-contains? [rng umap x]
  (let [{:keys [start step end]} (concretize-range rng umap)]
    (and (or (<= start x end)
             (>= start x end))
         (or (= start x)
             (= end x)
             (and (u/finite? start)
                  (-> x (- start) (mod step) zero?))
             (and (u/finite? end)
                  (-> x (+ end) (mod step) zero?))))))


(defn range-contains-some [rng umap & xs]
  (->> (sort xs)
       (filter (partial range-contains? rng umap))
       first))


(defn sequence-contains-some
  "Returns first (i.e. min) x found in the s"
  [useq umap x & xs]
  (let [xs (cons x xs)]
    (some (some-fn (set xs)
                   #(when (range? %) (apply range-contains-some % umap xs)))
          (:sequence useq))))


(defn range-index-of
  "Returns negative index if range start is infinite, 0 index will be end of range."
  [rng umap x]
  (let [{:as crng, :keys [start step end]} (concretize-range rng umap)]
    (cond
      (not (range-contains-some crng umap x))   nil
      (and (u/infinite? x) (u/infinite? start)) ##-Inf
      (u/infinite? x)                           ##Inf
      (u/infinite? start)                       (- (quot (- end x) step))
      :else                                     (quot (- x start) step))))


(defn sequence-index-of [useq umap x]
  (loop [i 0, [el & rest-s] (:sequence useq)]
    (when (some? el)
      (or (some-> (cond
                    (= x el)    0
                    (range? el) (range-index-of el umap x))
                  (+ i))
          (recur (+ i (if (and (range? el) (u/finite? (:start el)))
                        (range-length el umap)
                        1))
                 rest-s)))))


(defn range-nth [rng umap index]
  (let [{:keys [start step end]} (concretize-range rng umap)]
    (if (u/infinite? start)
      (+ end (* step index))
      (+ start (* step index)))))


(defn sequence-nth [useq umap index]
  (loop [i 0, [el & rest-s] (:sequence useq)]
    (when (some? el)
      (let [increment (if (and (range? el) (u/finite? (:start el)))
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


;;;;;;;;;; type


(defn get-next-unit
  "next = more significant"
  [umap unit]
  (u/get-next-element (first (guess-sys umap unit)) unit))


(defn get-prev-unit
  "prev = less significant"
  [umap unit]
  (u/get-prev-element (first (guess-sys umap unit)) unit))


(defn get-ctx-unit-seq [ctx unit next-unit]
  (get-in ctx [:seqs unit next-unit]))


(def get-unit-seq*
  (memoize
    (fn [unit next-unit]
      (get-ctx-unit-seq @ctx unit next-unit))))


(defn get-unit-seq [umap unit]
  (let [sys       (first (guess-sys umap unit))
        next-unit (u/get-next-element sys unit)]
    (get-unit-seq* unit next-unit)))


(defn sys-unit-seqs [sys]
  (map (fn [unit next-unit]
         [unit (get-unit-seq* unit next-unit)])
       sys
       (rest (conj sys nil))))


;;;;;;;;;; inc & dec


(defn get-next-unit-value [useq umap x]
  (loop [[el next & rest] (:sequence useq)]
    (let [{:keys [step end]} (if (range? el)
                               (concretize-range el umap)
                               {})]
      (cond
        (nil? el)
        nil

        (or (= x el) (= x end))
        (cond-> next (range? next) (-> :start (u/try-call umap)))

        (and (range? el)
             (range-contains-some el umap x)
             (range-contains-some el umap (+ x step)))
        (+ x step)

        :else
        (recur (cons next rest))))))


(defn get-prev-unit-value [useq umap x]
  (loop [[prev el & rest] (cons nil (:sequence useq))]
    (let [{:keys [start step]} (if (range? el) (concretize-range el umap) {})]
      (cond
        (nil? el)
        nil

        (or (= x el) (= x start))
        (cond-> prev (range? prev) (-> :end (u/try-call umap)))

        (and (range? el)
             (range-contains-some el umap x)
             (range-contains-some el umap (- x step)))
        (- x step)

        :else
        (recur (cons el rest))))))


(defn get-first-el [useq umap]
  (let [start (first (:sequence useq))]
    (if (range? start)
      (u/try-call (:start start) umap)
      start)))


(defn get-last-el [useq umap]
  (let [end (last (:sequence useq))]
    (if (range? end)
      (u/try-call (:end end) umap)
      end)))


(defn get-min-value [umap unit]
  (get-first-el (get-unit-seq umap unit) umap))


(defn get-max-value [umap unit]
  (get-last-el (get-unit-seq umap unit) umap))


#_"TODO: handle when get-next-unit returns nil"
(defn inc-unit [unit {:as umap, unit-value unit, :or {unit-value (get-min-value umap unit)}}]
  (or (some->> unit-value
               (get-next-unit-value (get-unit-seq umap unit) umap)
               (assoc umap unit))
      (as-> umap $
        (inc-unit (get-next-unit $ unit) $)
        (assoc $ unit (get-min-value $ unit)))))


(defn dec-unit [unit {:as umap, unit-value unit, :or {unit-value (get-min-value umap unit)}}]
  (or (some->> unit-value
               (get-prev-unit-value (get-unit-seq umap unit) umap)
               (assoc umap unit))
      (as-> umap $
        (dissoc $ unit)
        (dec-unit (get-next-unit $ unit) $)
        (assoc $ unit (get-max-value $ unit)))))


(defn add-to-unit [umap unit x] #_"TODO: handle unnormalized values"
  (cond
    (zero? x)
    umap

    (and (< 1 (abs x))
         (static-sequence? (get-unit-seq umap unit)))
    (let [useq        (get-unit-seq umap unit)
          idx         (if-let [v (get umap unit)]
                         (sequence-index-of useq umap v)
                         (sequence-first-index useq umap))
          sum         (+ idx x)
          modulo      (sequence-length useq umap)
          result-idx  (cond-> sum (u/finite? modulo) (mod modulo))
          carry-delta (if (u/infinite? modulo) 0 (u/floor (/ sum modulo)))
          result      (sequence-nth useq umap result-idx)
          result-umap (assoc umap unit result)]
      (if (zero? carry-delta)
        result-umap
        (recur result-umap
               (get-next-unit umap unit)
               carry-delta)))

    (neg? x)
    (u/n-times (- x) (partial dec-unit unit) umap)

    :else
    (u/n-times x (partial inc-unit unit) umap)))


(defn subtract-from-unit [umap unit x]
  (add-to-unit umap unit (- x)))


;;;;;;;;;; cmp
#_"TODO: add zero?"


(defn sequence-cmp [useq umap x y]
  (cond
    (= x y) 0
    (nil? x) -1
    (nil? y) 1
    (= x (sequence-contains-some useq umap x y)) -1
    :else 1))


(defn cmp-in-sys [sys x y]
  (or (->> (sys-unit-seqs sys)
           reverse
           (map (fn [[unit processed-sequence]]
                  (sequence-cmp processed-sequence
                                x
                                (get x unit)
                                (get y unit))))
           (drop-while zero?)
           first)
      0))


(defn cmp [x y]
  (or (when-let [sys (->> (sys-intersection x y)
                          first)]
        (cmp-in-sys sys x y))
      (when-let [sys (or (first (guess-sys x))
                         (first (guess-sys y)))]
        (cmp-in-sys sys x y))
      (when (= x y)
        0)))


(defn eq?
  ([_]          true)
  ([x y]        (= 0 (cmp x y)))
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


;;;;;;;;;; arithmetic


(defn add-delta
  ([] {})

  ([x] x)

  ([x delta]
   (reduce (fn [result unit]
             (add-to-unit result unit (get delta unit 0)))
           x
           (reverse (first (sys-intersection x delta)))))

  ([x delta & more-deltas]
   (reduce add-delta
           (add-delta x delta)
           more-deltas)))


(defn subtract-delta
  ([] {})

  ([x] x)

  ([x delta]
   (reduce (fn [result unit]
             (subtract-from-unit result unit (get delta unit 0)))
           x
           (reverse (first (sys-intersection x delta)))))

  ([x delta & more-deltas]
   (reduce subtract-delta
           (subtract-delta x delta)
           more-deltas)))


(defn unit-difference [a b unit useq]
  (let [a-val   (some->> (get a unit) (sequence-index-of useq a))
        b-val   (some->> (get b unit) (sequence-index-of useq b))
        diff    (- (or a-val 0) (or b-val 0))
        borrow? (neg? diff)]
    {:borrowed borrow?
     :result (if borrow?
               (let [borrow (sequence-length useq b)]
                 (+ diff borrow))
               diff)}))


(defn units-difference-reduce-fn [a b {:keys [acc borrow?]} [unit useq]]
  (let [{borrow-next? :borrowed, unit-res :result}
        (unit-difference a
                         (cond->> b borrow? (inc-unit unit))
                         unit
                         useq)]
    {:borrow? borrow-next?
     :acc (cond-> acc
            (not= 0 unit-res)
            (assoc unit unit-res))}))


(defn difference [x y]
  (let [[a b] (cond-> [x y] (lt? x y) reverse)]
    (:acc (reduce (partial units-difference-reduce-fn a b)
                  {}
                  (sys-unit-seqs (first (sys-intersection a b)))))))


#_(defn difference-parts [units sys-seqs]
  (:acc (reduce
          (fn [acc unit]
            (let [[seqs [this-unit & rest-seqs]]
                  (split-with (fn [[u _]] (not= unit u))
                              (:rest-seqs acc))]
              (if (some? this-unit)
                (-> acc
                    (assoc :rest-seqs rest-seqs)
                    (update :acc conj {:to-unit unit
                                       :seqs (conj seqs this-unit)}))
                acc)))
          {:acc []
           :rest-seqs (reverse sys-seqs)}
          (reverse units))))


#_(defn difference-in [units x y]
  (let [[a b]    (cond-> [x y] (lt? x y) reverse)
        sys-seqs (sys-unit-seqs (first (sys-intersection a b)))
        parts    (difference-parts units sys-seqs)]
    (reduce
      (fn [acc {:keys [to-unit seqs]}]
        (reduce
          (fn [{:keys [a b acc]} [unit useq]]
            (let [{:keys [borrowed result]}
                  (unit-difference a b unit useq)]
              {:acc (assoc acc unit result)
               :a (dissoc a unit)
               :b (dissoc b unit)}))
          acc
          seqs))
      {:a a
       :b b
       :acc {}}
      parts)))


; 2022-05-03 2019-07-28
;
; 31-28=3
; 2022-05-03 2019-08 3
; 3+31+30+31+30+31=156
; 2022-05-03 2020
;
; 156+366+365=887
; 2022-05-03 2022
;
; 887+3=890
; 2022-05 2022 890
;
; 890+31+28+31+30=1010
