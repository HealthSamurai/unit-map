(ns unit-map.systems.date-time.defs
  (:require [unit-map.core :as u]
            [unit-map.systems.date-time.misc :as misc]))


(u/defseq :ns   #unit-map/seq[0 1 .. 999999999 -> :sec])
(u/defseq :ms   #unit-map/seq[0 1 .. 999 -> :sec])
(u/defseq :sec  #unit-map/seq[0 1 .. 59 -> :min])
(u/defseq :min  #unit-map/seq[0 1 .. 59 -> :hour])
(u/defseq :hour #unit-map/seq[0 1 .. ##Inf])
(u/defseq :hour #unit-map/seq[0 1 .. 23 -> :day])

(u/defseq :hour   #unit-map/seq[12 1 2 .. 11 -> :period])
(u/defseq :period #unit-map/seq[:am :pm -> :day])

(u/defseq :day   #unit-map/seq[1 2 .. misc/days-in-month -> :month])
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
