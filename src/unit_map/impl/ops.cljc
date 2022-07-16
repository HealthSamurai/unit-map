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


(defn cmp [registry x y]
  (or (when (= x y)
        0)
      (when-let [system (system/system-intersection registry x y)]
        (cmp-in-system registry system x y))
      (when-let [system (or (system/guess-system registry x)
                            (system/guess-system registry y))]
        (cmp-in-system registry system x y))))


(defn eq? [registry x y]
  (= 0 (cmp registry x y)))


(defn lt? [registry x y]
  (neg? (cmp registry x y)))


(defn gt? [registry x y]
  (pos? (cmp registry x y)))


(defn lte? [registry x y]
  (>= 0 (cmp registry x y)))


(defn gte? [registry x y]
  (<= 0 (cmp registry x y)))


;;;;;;;;;; arithmetic


#_"TODO: handle when get-next-unit returns nil"
(defn inc-unit [registry unit {:as umap, unit-value unit}]
  (or (some->> (or unit-value (system/get-min-value registry umap unit))
               (system/get-next-unit-value (system/get-useq registry umap unit)
                                           umap)
               (assoc umap unit))
      (as-> umap $
        (inc-unit registry (system/get-next-unit registry $ unit) $)
        (assoc $ unit (system/get-min-value registry $ unit)))))


(defn dec-unit [registry unit {:as umap, unit-value unit}]
  (or (some->> (or unit-value (system/get-min-value registry umap unit))
               (system/get-prev-unit-value (system/get-useq registry umap unit)
                                           umap)
               (assoc umap unit))
      (as-> umap $
        (dissoc $ unit)
        (dec-unit registry (system/get-next-unit registry $ unit) $)
        (assoc $ unit (system/get-max-value registry $ unit)))))


#_"TODO: handle unnormalized values"
(defn add-to-unit [registry umap unit x]
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
          carry-delta (if (util/infinite? modulo)
                        0
                        (util/floor (/ sum modulo)))
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


(defn add-delta [registry umap delta]
  (reduce (fn [result unit]
            (add-to-unit registry result unit (get delta unit 0)))
          umap
          (reverse (system/system-intersection registry umap delta))))


(defn subtract-delta [registry umap delta]
  (reduce (fn [result unit]
            (subtract-from-unit
              registry result unit (get delta unit 0)))
          umap
          (reverse (system/system-intersection registry umap delta))))


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


(defn units-difference-reduce-fn [registry a b
                                  {:keys [acc borrow?]}
                                  [unit useq]]
  (let [{borrow-next? :borrowed, unit-res :result}
        (unit-difference a
                         (cond->> b borrow? (inc-unit registry unit))
                         unit
                         useq)]
    {:borrow? borrow-next?
     :acc (cond-> acc
            (not= 0 unit-res)
            (assoc unit unit-res))}))


(defn difference [registry x y]
  (let [[a b] (cond-> [x y] (lt? registry x y) reverse)]
    (:acc (reduce #(units-difference-reduce-fn registry a b %1 %2)
                  {}
                  (->> (system/system-intersection registry a b)
                       (system/system-useqs registry))))))


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


#_(defn difference-in [registry units x y]
    (let [[a b]    (cond-> [x y] (lt? x y) reverse)
          system-useqs (system-useqs registry (system-intersection a b))
          parts    (difference-parts units system-useqs)]
      (reduce
        (fn [acc {:keys [to-unit useqs]}]
          (reduce
            (fn [{:keys [a b acc]} [unit useq]]
              (let [{:keys [borrowed result]}
                    (unit-difference a b unit useq)]
                {:acc (assoc acc unit result)
                 :a (dissoc a unit)
                 :b (dissoc b unit)}))
            acc
            useqs))
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
