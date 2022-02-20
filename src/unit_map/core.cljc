(ns unit-map.core
  (:require [unit-map.ops :as op]
            [unit-map.type.chrono.datetime :as dt]
            [unit-map.type.chrono.date :as d]
            [unit-map.type.chrono.time :as t]
            [unit-map.type.chrono.util.misc :as um]
            [clojure.set]
            [clojure.data]))


(defonce ctx #_"TODO: can it be done without global atom state?"
  (atom nil))
#_(reset! ctx nil)


(defn range? [x]
  (and (map? x) (::range (meta x))))


(defn process-range [pprev prev next-seq nnext]
  {:pre [(and (not-every? nil? [pprev nnext])
              (every? some? [prev next-seq]))]}
  ^::range{:start (or pprev prev)
           :step  (if (nil? pprev) (- nnext next-seq) (- prev pprev))
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
