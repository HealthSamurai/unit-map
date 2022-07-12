(ns unit-map.systems.date-time.defs
  (:require [unit-map.core :as u]))


(defn leap-year? [{:keys [year]}]
  (and (zero? (rem year 4))
       (or (pos? (rem year 100))
           (zero? (rem year 400)))))


(defn days-in-month [{:as date, :keys [month year]}]
  (cond
    (some nil? [month year])            ##Inf
    (contains? #{4 6 9 11} month)       30
    (and (leap-year? date) (= 2 month)) 29
    (= 2 month)                         28
    :else                               31))


(def seqs
  [:ns   #unit-map/seq[0 1 .. 999999999 -> :sec]
   :ms   #unit-map/seq[0 1 .. 999 -> :sec]
   :sec  #unit-map/seq[0 1 .. 59 -> :min]
   :min  #unit-map/seq[0 1 .. 59 -> :hour]
   :hour #unit-map/seq[0 1 .. ##Inf]
   :hour #unit-map/seq[0 1 .. 23 -> :day]

   :hour   #unit-map/seq[12 1 2 .. 11 -> :period]
   :period #unit-map/seq[:am :pm -> :day]

   :day   #unit-map/seq[1 2 .. days-in-month -> :month]
   :month #unit-map/seq[1 2 .. 12 -> :year]
   :year  #unit-map/seq[##-Inf .. -2 -1 1 2 .. ##Inf]])


(def systems
  [:date [:day :month :year]

   :ms-year [:ms :sec :min :hour :day :month :year]
   :ns-year [:ns :sec :min :hour :day :month :year]

   :am-pm-ms-year [:ms :sec :min :hour :period :day :month :year]
   :am-pm-ns-year [:ns :sec :min :hour :period :day :month :year]])


#_"NOTE: not sure if this system is needed by default"
#_(u/regseq! :ns #unit-map/seq[0 1 .. 999999 -> :ms])
#_(u/regsys! ns-ms-year [:ns :ms :sec :min :hour :day :month :year])
#_(u/regsys! am-pm-ns-ms-year [:ns :ms :sec :min :hour :period :day :month :year])
