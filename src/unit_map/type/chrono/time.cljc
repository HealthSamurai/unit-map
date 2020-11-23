(ns unit-map.type.chrono.time
  (:require [unit-map.ops :as ops]
            [unit-map.io :as io]))


(def military
  #unit-map/definition[:ms   [0 1 .. 999]
                       :sec  [0 1 .. 59]
                       :min  [0 1 .. 59]
                       :hour [0 1 .. 23]])


(defmethod ops/definition ::military [_] military)


(def am-pm
  #unit-map/definition [:ms     [0 1 .. 999]
                        :sec    [0 1 .. 59]
                        :min    [0 1 .. 59]
                        :hour   [12 1 2 .. 11]
                        :period [:am :pm]])


(defmethod ops/definition ::am-pm [_] am-pm)

(defmethod io/field-settings :ms [_]
  {:default-pad-width 3})

(defmethod io/field-settings :sec [_]
  {:default-pad-width 2})

(defmethod io/field-settings :min [_]
  {:default-pad-width 2})

(defmethod io/field-settings :hour [_]
  {:default-pad-width 2})
