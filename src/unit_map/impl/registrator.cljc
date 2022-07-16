(ns unit-map.impl.registrator
  (:require [unit-map.impl.registry :as registry]))


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
           (assert (every? #(registry/system-continuous? registry %)
                           systems))
           (registry/reg-systems registry systems))))

