(ns unit-map.impl.reader)


(defn create-range [start end step]
  {:start start
   :end   end
   :step  step})


(defn process-range [pprev prev next-seq nnext]
  {:pre [(and (not-every? nil? [pprev nnext])
              (every? some? [prev next-seq]))]}
  (let [start (or pprev prev)
        end   (or nnext next-seq)
        step  (if (nil? pprev)
                (if (integer? next-seq)
                  (- nnext next-seq)
                  next-seq)
                (if (integer? prev)
                  (- prev pprev)
                  prev))]
    (create-range start end step)))


(defn process-enumeration [s]
  (loop [[pprev prev x next-seq nnext & rest] (concat [nil nil] s [nil nil])

         result []
         buffer []]
    (cond
      (nil? x)  (vec (concat result buffer))
      (= '.. x) (recur (concat [nil nil] rest)
                       (concat result
                               (drop-last 2 buffer)
                               [(process-range pprev prev next-seq nnext)])
                       [])
      :else     (recur (concat [prev x next-seq nnext] rest)
                       result
                       (conj buffer x)))))


(defn process-sequence* [s]
  (process-enumeration s))


(def process-sequence (memoize process-sequence*))


(defn read-sequence [form]
  {:useq (process-sequence form)})
