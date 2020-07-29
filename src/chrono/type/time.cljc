(ns chrono.type.time
  (:require [chrono.new-ops :as ops]))

(def military
  (array-map
   :ms     [0 1 '.. 999]
   :sec    [0 1 '.. 59]
   :min    [0 1 '.. 59]
   :hour   [0 1 '.. 23]))

(defmethod ops/definition ::military [_] military)


(def am-pm
  (array-map
   :ms     [0 1 '.. 999]
   :sec    [0 1 '.. 59]
   :min    [0 1 '.. 59]
   :hour   [12 1 2 '.. 11]
   :period [:am :pm]))

(defmethod ops/definition ::am-pm [_] am-pm)
