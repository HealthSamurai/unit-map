(ns unit-map.impl.reader
  (:require [unit-map.impl.system :as system]))


(defn process-urange [pprev prev next-useq nnext]
  {:pre [(and (not-every? nil? [pprev nnext])
              (every? some? [prev next-useq]))]}
  (let [start (or pprev prev)
        end   (or nnext next-useq)
        step  (if (nil? pprev)
                (if (integer? next-useq)
                  (- nnext next-useq)
                  next-useq)
                (if (integer? prev)
                  (- prev pprev)
                  prev))]
    (system/create-urange start end step)))


(defn process-enumeration [s]
  (loop [[pprev prev x next-useq nnext & rest] (concat [nil nil] s [nil nil])

         result []
         buffer []]
    (cond
      (nil? x)  (vec (concat result buffer))
      (= '.. x) (recur (concat [nil nil] rest)
                       (concat result
                               (drop-last 2 buffer)
                               [(process-urange pprev prev next-useq nnext)])
                       [])
      :else     (recur (concat [prev x next-useq nnext] rest)
                       result
                       (conj buffer x)))))


(defn process-useq* [s]
  (process-enumeration s))


(def process-useq (memoize process-useq*))


(defn read-useq [form]
  (process-useq form))
