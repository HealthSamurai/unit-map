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


(def ns->sec   {:unit :ns,     :next-unit :sec,  :useq #unit-map/useq[0 1 .. 999999999]})
(def ns->ms    {:unit :ns,     :next-unit :ms,   :useq #unit-map/useq[0 1 .. 999999]})
(def ms->sec   {:unit :ms,     :next-unit :sec,  :useq #unit-map/useq[0 1 .. 999]})
(def sec->min  {:unit :sec,    :next-unit :min,  :useq #unit-map/useq[0 1 .. 59]})
(def min->hour {:unit :min,    :next-unit :hour, :useq #unit-map/useq[0 1 .. 59]})
(def hour->day {:unit :hour,   :next-unit :day,  :useq #unit-map/useq[0 1 .. 23]})

(def hours {:unit :hour, :useq #unit-map/useq[0 1 .. ##Inf]})
(def days  {:unit :day,  :useq #unit-map/useq[0 1 .. ##Inf]})

(def hour->period {:unit :hour,   :next-unit :period, :useq #unit-map/useq[12 1 2 .. 11]})
(def period->day  {:unit :period, :next-unit :day,    :useq #unit-map/useq[:am :pm]})

(def day->month  {:unit :day,   :next-unit :month, :useq #unit-map/useq[1 2 .. days-in-month]})
(def month->year {:unit :month, :next-unit :year,  :useq #unit-map/useq[1 2 .. 12]})
(def years       {:unit :year,                     :useq #unit-map/useq[##-Inf .. -2 -1 1 2 .. ##Inf]})


(def date [:day :month :year])

(def ms-hour    [:ms :sec :min :hour])
(def ns-hour    [:ns :sec :min :hour])
(def ns-ms-hour [:ns :ms :sec :min :hour])

(def ms-day    [:ms :sec :min :hour :day])
(def ns-day    [:ns :sec :min :hour :day])
(def ns-ms-day [:ns :ms :sec :min :hour :day])

(def ms-year    [:ms :sec :min :hour :day :month :year])
(def ns-year    [:ns :sec :min :hour :day :month :year])
(def ns-ms-year [:ns :ms :sec :min :hour :day :month :year])

(def am-pm-ms-year    [:ms :sec :min :hour :period :day :month :year])
(def am-pm-ns-year    [:ns :sec :min :hour :period :day :month :year])
(def am-pm-ns-ms-year [:ns :ms :sec :min :hour :period :day :month :year])


(def useqs
  #{ns->sec ns->ms ms->sec sec->min min->hour hour->day
    hours days
    hour->period period->day
    day->month month->year years})


(def systems
  #{date
    ms-hour ns-hour
    ms-day ns-day
    ms-year ns-year
    am-pm-ms-year am-pm-ns-year})


#_(let [reg (unit-map.core/new-registry)]
    (unit-map.core/reg-useqs! reg useqs)
    (unit-map.core/reg-usyss! reg systems))

#_"NOTE: not sure if this system is needed by default"
#_[:ns :ms :sec :min :hour :day :month :year]
#_[:ns :ms :sec :min :hour :period :day :month :year]
