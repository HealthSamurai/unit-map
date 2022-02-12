(ns unit-map.core
  (:require [unit-map.ops :as op]
            [unit-map.type.chrono.datetime :as dt]
            [unit-map.type.chrono.date :as d]
            [unit-map.type.chrono.time :as t]
            [unit-map.type.chrono.util.misc :as um]
            [clojure.set]))


(defn range? [x]
  (and (map? x) (:range (meta x))))


(defn process-range [pprev prev next-seq nnext]
  {:pre [(and (not-every? nil? [pprev nnext])
              (every? some? [prev next-seq]))]}
  ^:range{:start (or pprev prev)
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


(defonce seqs (atom {}))
#_(reset! seqs {})


(defn push-to-seq-graph [seqs-map unit unit-seq]
  (let [eq-seqs (when-let [eq-unit (:eq-unit unit-seq)]
                  (->> (vals seqs-map)
                       (filter #(get % eq-unit))
                       (mapcat vals)))

        to-this-unit-new (->> eq-seqs
                              (map #(assoc % :next-unit unit))
                              distinct)

        to-this-unit (->> (vals seqs-map)
                          (filter #(get % unit))
                          (mapcat vals))

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


(defmacro defseq [unit unit-seq]
  (swap! seqs #(push-to-seq-graph % unit unit-seq))
  unit-seq)


(defonce systems (atom {}))
#_(reset! systems {})


(defn sys-continuous? [units]
  (let [reverse-units (reverse units)]
    (->> (map vector
              reverse-units
              (rest reverse-units))
         (every?
           (fn [[cur-unit prev-unit]]
             (get-in @seqs [prev-unit cur-unit]))))))


(defmacro defsys [sys-name units]
  (assert (sys-continuous? units))
  (let [sys-def `(def ~sys-name ~units)]
    (swap! systems assoc sys-name units)
    sys-def))


(defn get-units [unit-map]
  (->> unit-map
       (mapcat (fn [[k v]]
                 (if (map? v)
                   (get-units v)
                   [k])))
       set))


(defn guess-sys [unit-map]
  (when-let [units (not-empty (get-units unit-map))]
    (->> (vals @systems)
         (filter (comp (partial clojure.set/subset? units)
                       set))
         sort)))


(defn sys-intersection [& unit-maps]
  (->> unit-maps
       (map (comp set guess-sys))
       (apply clojure.set/intersection)
       sort))
