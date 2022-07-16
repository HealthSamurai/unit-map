(ns unit-map.core
  (:refer-clojure :exclude [format])
  (:require [unit-map.impl.util :as util]
            [unit-map.impl.registrator :as registrator]
            [unit-map.impl.system :as system]
            [unit-map.impl.ops :as ops]
            [unit-map.impl.io :as io]))


#_"TODO:
- guess-system-with-useqs
- refactor repeating guess-system calls
- maybe use plural for deltas e.g.: {:month :jul} and {:months 7}
- move calendar, mask & crono to scripts"


;;;;;;;;;; registrator


(defn new-registry []
  (registrator/new-registry))


(defn reg-useq! [registry-ref & {:keys [unit useq next-unit eq-unit]}]
  (registrator/reg-useq!
    registry-ref
    {:unit unit
     :useq useq
     :next-unit next-unit
     :eq-unit eq-unit}))


(defn reg-useqs! [registry-ref useqs]
  (registrator/reg-useqs! registry-ref useqs))


(defn reg-system! [registry-ref units]
  (registrator/reg-system! registry-ref units))


(defn reg-systems! [registry-ref systems]
  (registrator/reg-systems! registry-ref systems))


;;;;;;;;;; parse & format string


(defn parse [s fmt-vec & {:keys [strict]}]
  (io/parse s fmt-vec :strict strict))


(defn format [t fmt-vec]
  (io/format t fmt-vec))


;;;;;;;;;; compare


(defn cmp [registry x y]
  (ops/cmp registry x y))


(defn eq?
  ([_registry _x] true)

  ([registry x y]
   (ops/eq? registry x y))

  ([registry x y & more]
   (apply util/apply-binary-pred
          #(ops/eq? registry  %1 %2)
          x y more)))


(def not-eq? (complement eq?))


(defn lt?
  ([_registry _x] true)

  ([registry x y]
   (ops/lt? registry x y))

  ([registry x y & more]
   (apply util/apply-binary-pred
          #(ops/lt? registry  %1 %2)
          x y more)))


(defn gt?
  ([_registry _x] true)

  ([registry x y]
   (ops/gt? registry x y))

  ([registry x y & more]
   (apply util/apply-binary-pred
          #(ops/gt? registry  %1 %2)
          x y more)))


(defn lte?
  ([_registry _x] true)

  ([registry x y]
   (ops/lte? registry x y))

  ([registry x y & more]
   (apply util/apply-binary-pred
          #(ops/lte? registry  %1 %2)
          x y more)))


(defn gte?
  ([_registry _x] true)

  ([registry x y]
   (ops/gte? registry x y))

  ([registry x y & more]
   (apply util/apply-binary-pred
          #(ops/gte? registry  %1 %2)
          x y more)))


;;;;;;;;;; arithmetic


(defn add-delta
  ([_registry] {})

  ([_registry x] x)

  ([registry x delta]
   (ops/add-delta registry x delta))

  ([registry x delta & more-deltas]
   (reduce #(ops/add-delta registry %1 %2)
           (ops/add-delta registry x delta)
           more-deltas)))


(defn subtract-delta
  ([_registry] {})

  ([_registry x] x)

  ([registry x delta]
   (ops/subtract-delta registry x delta))

  ([registry x delta & more-deltas]
   (reduce #(ops/subtract-delta registry %1 %2)
           (ops/subtract-delta registry x delta)
           more-deltas)))


(defn difference [registry x y]
  (ops/difference registry x y))
