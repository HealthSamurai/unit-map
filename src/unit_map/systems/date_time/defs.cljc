(ns unit-map.systems.date-time.defs
  (:require [unit-map.core :as u]))


(defn leap-year? [year]
  (and (zero? (rem year 4))
       (or (pos? (rem year 100))
           (zero? (rem year 400)))))


(defn days-in-month [{:keys [month year]}]
  (cond
    (some nil? [month year])            ##Inf
    (contains? #{4 6 9 11} month)       30
    (and (leap-year? year) (= 2 month)) 29
    (= 2 month)                         28
    :else                               31))


(u/defseq :ns   #unit-map/seq[0 1 .. 999999999 -> :sec])
(u/defseq :ms   #unit-map/seq[0 1 .. 999 -> :sec])
(u/defseq :sec  #unit-map/seq[0 1 .. 59 -> :min])
(u/defseq :min  #unit-map/seq[0 1 .. 59 -> :hour])
(u/defseq :hour #unit-map/seq[0 1 .. ##Inf])
(u/defseq :hour #unit-map/seq[0 1 .. 23 -> :day])

(u/defseq :hour   #unit-map/seq[12 1 2 .. 11 -> :period])
(u/defseq :period #unit-map/seq[:am :pm -> :day])

(u/defseq :day   #unit-map/seq[1 2 .. days-in-month -> :month])
(u/defseq :month #unit-map/seq[1 2 .. 12 -> :year])
(u/defseq :year  #unit-map/seq[##-Inf .. -2 -1 1 2 .. ##Inf])


(u/defsys :date [:day :month :year])

(u/defsys :ms-year [:ms :sec :min :hour :day :month :year])
(u/defsys :ns-year [:ns :sec :min :hour :day :month :year])

(u/defsys :am-pm-ms-year [:ms :sec :min :hour :period :day :month :year])
(u/defsys :am-pm-ns-year [:ns :sec :min :hour :period :day :month :year])


#_"NOTE: not sure if this system is needed by default"
#_(u/defseq :ns #unit-map/seq[0 1 .. 999999 -> :ms])
#_(u/defsys ns-ms-year [:ns :ms :sec :min :hour :day :month :year])
#_(u/defsys am-pm-ns-ms-year [:ns :ms :sec :min :hour :period :day :month :year])
