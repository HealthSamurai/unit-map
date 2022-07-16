(ns unit-map.impl.ops
  (:require [unit-map.impl.util :as util]
            [unit-map.impl.reader]
            [unit-map.impl.registry :as registry]
            [unit-map.impl.system :as system]
            [clojure.set]
            [clojure.data]))

;;;;;;;;;; cmp
#_"TODO: add zero?"


(defn useq-cmp [useq umap x y]
  (cond
    (= x y) 0
    (nil? x) -1
    (nil? y) 1
    (= x (system/useq-contains-some useq umap x y)) -1
    :else 1))


(defn cmp-in-system [registry system x y]
  (or (->> (system/system-useqs registry system)
           reverse
           (map (fn [[unit processed-useq]]
                  (useq-cmp processed-useq
                            x
                            (get x unit)
                            (get y unit))))
           (drop-while zero?)
           first)
      0))


;;;;;;;;;; arithmetic


#_"TODO: handle when get-next-unit returns nil"
(defn inc-unit [registry unit {:as umap, unit-value unit}]
  (or (some->> (or unit-value (system/get-min-value registry umap unit))
               (system/get-next-unit-value (system/get-useq registry umap unit) umap)
               (assoc umap unit))
      (as-> umap $
        (inc-unit registry (system/get-next-unit registry $ unit) $)
        (assoc $ unit (system/get-min-value registry $ unit)))))


(defn dec-unit [registry unit {:as umap, unit-value unit}]
  (or (some->> (or unit-value (system/get-min-value registry umap unit))
               (system/get-prev-unit-value (system/get-useq registry umap unit) umap)
               (assoc umap unit))
      (as-> umap $
        (dissoc $ unit)
        (dec-unit registry (system/get-next-unit registry $ unit) $)
        (assoc $ unit (system/get-max-value registry $ unit)))))


(defn add-to-unit [registry umap unit x] #_"TODO: handle unnormalized values"
  (cond
    (zero? x)
    umap

    (and (< 1 (abs x))
         (system/static-useq? (system/get-useq registry umap unit)))
    (let [useq        (system/get-useq registry umap unit)
          idx         (if-let [v (get umap unit)]
                        (system/useq-index-of useq umap v)
                        (system/useq-first-index useq umap))
          sum         (+ idx x)
          modulo      (system/useq-length useq umap)
          result-idx  (cond-> sum (util/finite? modulo) (mod modulo))
          carry-delta (if (util/infinite? modulo) 0 (util/floor (/ sum modulo)))
          result      (system/useq-nth useq umap result-idx)
          result-umap (assoc umap unit result)]
      (if (zero? carry-delta)
        result-umap
        (recur registry
               result-umap
               (system/get-next-unit registry umap unit)
               carry-delta)))

    (neg? x)
    (util/n-times (- x) (partial dec-unit registry unit) umap)

    :else
    (util/n-times x (partial inc-unit registry unit) umap)))


(defn subtract-from-unit [registry umap unit x]
  (add-to-unit registry umap unit (- x)))


(defn unit-difference [a b unit useq]
  (let [a-val   (some->> (get a unit) (system/useq-index-of useq a))
        b-val   (some->> (get b unit) (system/useq-index-of useq b))
        diff    (- (or a-val 0) (or b-val 0))
        borrow? (neg? diff)]
    {:borrowed borrow?
     :result (if borrow?
               (let [borrow (system/useq-length useq b)]
                 (+ diff borrow))
               diff)}))


(defn units-difference-reduce-fn [registry a b {:keys [acc borrow?]} [unit useq]]
  (let [{borrow-next? :borrowed, unit-res :result}
        (unit-difference a
                         (cond->> b borrow? (inc-unit registry unit))
                         unit
                         useq)]
    {:borrow? borrow-next?
     :acc (cond-> acc
            (not= 0 unit-res)
            (assoc unit unit-res))}))


#_(defn difference-parts [units system-useqs]
    (:acc (reduce
            (fn [acc unit]
              (let [[useqs [this-unit & rest-useqs]]
                    (split-with (fn [[u _]] (not= unit u))
                                (:rest-useqs acc))]
                (if (some? this-unit)
                  (-> acc
                      (assoc :rest-useqs rest-useqs)
                      (update :acc conj {:to-unit unit
                                         :useqs (conj useqs this-unit)}))
                  acc)))
            {:acc []
             :rest-useqs (reverse system-useqs)}
            (reverse units))))
