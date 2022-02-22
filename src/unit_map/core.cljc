(ns unit-map.core
  (:require [unit-map.util :as u]
            [clojure.set]
            [clojure.data]))


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
  (defseq* ctx unit unit-seq))


(defn sys-continuous?* [ctx units]
  (let [reverse-units (reverse units)]
    (->> (map vector
              reverse-units
              (rest reverse-units))
         (every?
           (fn [[cur-unit prev-unit]]
             (get-in @ctx [:seqs prev-unit cur-unit]))))))


(defn sys-continuous? [units]
  (sys-continuous?* ctx units))


(defn defsys* [ctx sys-name units]
  (assert (sys-continuous?* ctx units))
  (let [sys-def `(def ~sys-name ~units)]
    (swap! ctx assoc-in [:systems sys-name] units)
    sys-def))


(defmacro defsys [sys-name units]
  (defsys* ctx sys-name units))


;;;;;;;;;; sys info


(defn get-units [unit-map]
  (->> unit-map
       (mapcat (fn [[k v]]
                 (if (map? v)
                   (get-units v)
                   [k])))
       set))


(defn guess-sys [unit-map]
  (when-let [units (not-empty (get-units unit-map))]
    (->> (vals (:systems @ctx))
         (filter (comp (partial clojure.set/subset? units)
                       set))
         sort)))


(defn sys-intersection* [& syss]
  (->> syss
       (map set)
       (apply clojure.set/intersection)
       sort))


(defn sys-intersection [& unit-maps]
  (->> unit-maps
       (map guess-sys)
       (apply sys-intersection*)))


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
        branches-diff (or (first (sys-intersection* x-syss y-syss))
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


(defn dynamic-sequence? [sq]
  (boolean (some #(and (range? %)
                       (some fn? (vals %)))
                 (:sequence sq))))


(defn static-sequence? [ps]
  (not (dynamic-sequence? ps)))


(defn concretize-range [rng value]
  (u/map-v #(u/try-call % value)
           rng))


(defn range-length [rng value]
  (let [{:keys [start step end]} (concretize-range rng value)]
    (if (some u/infinite? [start end])
      ##Inf
      (-> (- end start) (quot step) inc))))


(defn sequence-length [ps value]
  (->> (:sequence ps)
       (map #(if (range? %) (range-length % value) 1))
       (reduce + 0)))


(defn sequence-first-index [ps value]
  (let [e (first (:sequence ps))
        r (when (range? e) (concretize-range e value))]
    (cond
      (nil? e)                 nil
      (u/infinite? (:start r)) ##-Inf
      :else                    0)))


(defn sequence-last-index [ps value]
  (let [e (last (:sequence ps))
        r (when (range? e) (concretize-range e value))]
    (cond
      (nil? e)               nil
      (u/infinite? (:end r)) ##Inf
      :else                  (dec (sequence-length ps value)))))


(defn range-contains? [rng value x]
  (let [{:keys [start step end]} (concretize-range rng value)]
    (and (or (<= start x end)
             (>= start x end))
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
  "Returns first (i.e. min) x found in the s"
  [ps value x & xs]
  (let [xs (cons x xs)]
    (some (some-fn (set xs)
                   #(when (range? %) (apply range-contains-some % value xs)))
          (:sequence ps))))


(defn range-index-of
  "Returns negative index if range start is infinite, 0 index will be end of range."
  [rng value x]
  (let [{:keys [start step end]} (concretize-range rng value)]
    (cond
      (not (range-contains-some rng value x))   nil
      (and (u/infinite? x) (u/infinite? start)) ##-Inf
      (u/infinite? x)                           ##Inf
      (u/infinite? start)                       (- (quot (- end x) step))
      :else                                     (quot (- x start) step))))


(defn sequence-index-of [ps value x]
  (loop [i 0, [el & rest-s] (:sequence ps)]
    (when (some? el)
      (or (some-> (cond
                    (= x el)    0
                    (range? el) (range-index-of el value x))
                  (+ i))
          (recur (+ i (if (and (range? el) (u/finite? (:start el)))
                        (range-length el value)
                        1))
                 rest-s)))))
