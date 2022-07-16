(ns unit-map.core
  (:refer-clojure :exclude [format])
  (:require [unit-map.impl.util :as util]
            [unit-map.impl.reader]
            [unit-map.impl.registry :as registry]
            [unit-map.impl.system :as system]
            [unit-map.impl.ops :as ops]
            [unit-map.impl.io :as io]
            [clojure.string :as str]))


#_"TODO:
- guess-system-with-useqs
- refactor repeating guess-system calls
- use plural of unit for deltas (intervals)? e.g.: {:month :jul} and {:months 7}
- move calendar, mask & crono to scripts"


;;;;;;;;;; reg-useq! & reg-system!


(defn new-registry
  ([] (new-registry {}))

  ([init] (atom init)))


(defn reg-useq! [registry-atom & {:keys [unit useq next-unit eq-unit]}]
  (swap! registry-atom registry/reg-useq {:unit unit
                                          :useq useq
                                          :next-unit next-unit
                                          :eq-unit eq-unit})
  useq)


(defn reg-useqs! [registry-atom useqs]
  (swap! registry-atom registry/reg-useqs useqs))


(defn reg-system! [registry-atom units]
  (swap! registry-atom
         (fn [registry]
           (assert (registry/system-continuous? registry units))
           (registry/reg-system registry units)))
  units)


(defn reg-systems! [registry-atom systems]
  (swap! registry-atom
         (fn [registry]
           (assert (every? #(registry/system-continuous? registry %) systems))
           (registry/reg-systems registry systems))))


;;;;;;;;;; parse & format string


(defn parse [s fmt-vec & {:keys [strict]}]
  (io/parse s fmt-vec :strict strict))


(defn format [t fmt-vec]
  (io/format t fmt-vec))


;;;;;;;;;; compare


(defn cmp [registry x y]
  (or (when-let [system (system/system-intersection registry x y)]
        (ops/cmp-in-system registry system x y))
      (when-let [system (or (system/guess-system registry x)
                            (system/guess-system registry y))]
        (ops/cmp-in-system registry system x y))
      (when (= x y)
        0)))


(defn eq?
  ([_ _] true)
  ([registry x y]        (= 0 (cmp registry x y)))
  ([registry x y & more] (apply util/apply-binary-pred #(eq? registry  %1 %2) x y more)))


(def not-eq? (complement eq?))


(defn lt?
  ([_ _] true)
  ([registry x y]        (neg? (cmp registry x y)))
  ([registry x y & more] (apply util/apply-binary-pred #(lt? registry  %1 %2) x y more)))


(defn gt?
  ([_ _] true)
  ([registry x y]        (pos? (cmp registry x y)))
  ([registry x y & more] (apply util/apply-binary-pred #(gt? registry  %1 %2) x y more)))


(defn lte?
  ([_ _] true)
  ([registry x y]        (>= 0 (cmp registry x y)))
  ([registry x y & more] (apply util/apply-binary-pred #(lte? registry  %1 %2) x y more)))


(defn gte?
  ([_ _] true)
  ([registry x y]        (<= 0 (cmp registry x y)))
  ([registry x y & more] (apply util/apply-binary-pred #(gte? registry  %1 %2) x y more)))


;;;;;;;;;; arithmetic


(defn add-delta
  ([_registry] {})

  ([_registry x] x)

  ([registry x delta]
   (reduce (fn [result unit]
             (ops/add-to-unit registry result unit (get delta unit 0)))
           x
           (reverse (system/system-intersection registry x delta))))

  ([registry x delta & more-deltas]
   (reduce #(add-delta registry %1 %2)
           (add-delta registry x delta)
           more-deltas)))


(defn subtract-delta
  ([_registry] {})

  ([_registry x] x)

  ([registry x delta]
   (reduce (fn [result unit]
             (ops/subtract-from-unit registry result unit (get delta unit 0)))
           x
           (reverse (system/system-intersection registry x delta))))

  ([registry x delta & more-deltas]
   (reduce #(subtract-delta registry %1 %2)
           (subtract-delta registry x delta)
           more-deltas)))


(defn difference [registry x y]
  (let [[a b] (cond-> [x y] (lt? registry x y) reverse)]
    (:acc (reduce #(ops/units-difference-reduce-fn registry a b %1 %2)
                  {}
                  (system/system-useqs registry (system/system-intersection registry a b))))))


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
