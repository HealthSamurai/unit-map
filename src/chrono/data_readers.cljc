(ns chrono.data-readers
  (:require [chrono.new-ops :as ops])
  (:refer-clojure :exclude [sequence]))


(defn sequence [form]
  (vec (ops/process-sequence form)))


(defn definition [form]
  (apply array-map (mapcat (fn [[u s]] [u (sequence s)]) (partition 2 form))))
