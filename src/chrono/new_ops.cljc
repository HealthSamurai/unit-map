(ns chrono.new-ops
  (:require [chrono.util :as u]))

(defmulti rules (comp ffirst meta))

(defmethod rules :default [_]
  (array-map
   :ms     [0 1 '.. 999]
   :sec    [0 1 '.. 59]
   :min    [0 1 '.. 59]
   :hour   [0 1 '.. 23]
   :day    [1 2 '.. u/days-in-month]
   :month  [:jan :feb :mar :apr :may :jun :jul :aug :sep :oct :nov :dec]
   :year   [##-Inf '.. -2 -1 1 2 '.. ##Inf]))

(defmethod rules :am-pm [_]
  (array-map
   :ms     [0 1 '.. 999]
   :sec    [0 1 '.. 59]
   :min    [0 1 '.. 59]
   :hour   [12 1 2 '.. 11]
   :period [:am :pm]
   :day    [1 2 '.. u/days-in-month]
   :month  [:jan :feb :mar :apr :may :jun :jul :aug :sep :oct :nov :dec]
   :year   [##-Inf '.. -2 -1 1 2 '.. ##Inf]))

(defn process-range [pprev prev op next nnext]
  {:start (or pprev prev)
   :step  (if (nil? pprev) (- nnext next) (- prev pprev))
   :end   (or nnext next)})

(defn process-sequence [s]
  (loop [[pprev prev x next nnext & rest] (concat [nil nil] s [nil nil])
         result []
         buffer []]
    (cond
      (nil? x)  (concat result buffer)
      (= '.. x) (recur rest
                       (concat result
                               (drop-last 2 buffer)
                               [(process-range pprev prev x next nnext)])
                       [])
      :else     (recur (concat [prev x next nnext] rest)
                       result
                       (conj buffer x)))))

(defn range-contains? [{:keys [start step end]} x]
  (and (<= start x end)
       (or (= start x)
           (= end x)
           (and (not= ##-Inf start)
                (-> x (- start) (mod step) zero?))
           (and (not= ##Inf end)
                (-> x (+ end) (mod step) zero?)))))
