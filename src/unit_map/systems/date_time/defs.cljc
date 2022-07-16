(ns unit-map.systems.date-time.defs)


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


(def useqs
  [:ns   #unit-map/useq[0 1 .. 999999999] :next :sec
   :ms   #unit-map/useq[0 1 .. 999] :next :sec
   :sec  #unit-map/useq[0 1 .. 59] :next :min
   :min  #unit-map/useq[0 1 .. 59] :next :hour
   :hour #unit-map/useq[0 1 .. ##Inf]
   :hour #unit-map/useq[0 1 .. 23] :next :day

   :hour   #unit-map/useq[12 1 2 .. 11] :next :period
   :period #unit-map/useq[:am :pm] :next :day

   :day   #unit-map/useq[1 2 .. days-in-month] :next :month
   :month #unit-map/useq[1 2 .. 12] :next :year
   :year  #unit-map/useq[##-Inf .. -2 -1 1 2 .. ##Inf]])


(def systems
  [:date [:day :month :year]

   :ms-year [:ms :sec :min :hour :day :month :year]
   :ns-year [:ns :sec :min :hour :day :month :year]

   :am-pm-ms-year [:ms :sec :min :hour :period :day :month :year]
   :am-pm-ns-year [:ns :sec :min :hour :period :day :month :year]])


#_"NOTE: not sure if this system is needed by default"
#_(u/reguseq! :ns #unit-map/useq[0 1 .. 999999] :next :ms)
#_(u/regsys! ns-ms-year [:ns :ms :sec :min :hour :day :month :year])
#_(u/regsys! am-pm-ns-ms-year [:ns :ms :sec :min :hour :period :day :month :year])
