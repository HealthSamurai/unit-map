(ns chrono.calendar
  (:require [clojure.pprint]))


(defn datetime [t]
  (merge {:type :datetime
          :year 1900
          :month 1
          :day 1} t))

(def month-length
  {1 31
   2 {true 28 false 29}
   3 31
   4 30
   5 31
   6 30
   7 31
   8 31
   9 30
   10 31
   11 30
   12 31})

(defn is-leap? [y]
  (if (= 0 (rem y 100))
    (= 0 (rem y 400))
    (= 0 (rem y 4))))

(defn number-of-days [y m]
  (if (not (= 2 m))
    (get month-length m)
    (if (is-leap? y) 29 28)))

(defn days-and-months [y m d]
  (if (<= 1 d 27)
    [y m d]
    (cond
      (> d 0)
      (let [num-days (number-of-days y m)
            dd (- d num-days)]
        (if (<= d num-days)
          [y m d]
          (if (= m 12)
            (days-and-months (inc y) 1 dd)
            (days-and-months y (inc m) dd))))

      (<= d 0)
      (let [[num-days ny nm] (if (= m 1)
                               [(number-of-days (dec y) 12) (dec y) 12]
                               [(number-of-days y (dec m)) y (dec m)])
            dd (+ num-days d)]
        (if (< 0 dd)
          [ny nm dd]
          (days-and-months ny nm dd))))))

(defn plus
  [{ms' :ms s' :sec m' :min h' :hour d' :day mm' :month y' :year :as t}
   {ms'' :ms s'' :sec m'' :min h'' :hour d'' :day mm'' :month y'' :year :as i}]
  (let [ms      (when (or ms' ms'') (+ (or ms' 0) (or ms'' 0)))
        [ms s+] (when ms [(rem ms 1000) (quot ms 1000)])

        s (when (or s' s'' s+) (+ (or s' 0) (or s'' 0) (or s+ 0)))

        [s m+] (when s [(rem s 60) (quot s 60)])

        m (when (or m' m'' m+) (+ (or m' 0) (or m'' 0) (or m+ 0)))

        [m h+] (when m [(rem m 60) (quot m 60)])

        h (when (or h' h'' h+) (+ (or h' 0) (or h'' 0) (or h+ 0)))

        [h d+] (when h [(rem h 24) (quot h 24)])

        d (when (or d' d'' d+) (+ (or d' 0) (or d'' 0) (or d+ 0)))

        [y' mm' d] (when (and d y' mm')
                     (if (< 0 d 28)
                       [y' mm' d]
                       (days-and-months y' mm' d)))

        mm (when (or mm' mm'') (+ (or mm' 0) (or mm'' 0)))

        [mm y+] (when mm [(if (= 0 (rem mm 12) ) 12 (rem mm 12))
                          (if (= 0 (rem mm 12))
                            (dec (quot mm 12))
                            (quot mm 12))])

        y (when (or y' y'' y+) (+ (or y' 0) (or y'' 0) (or y+ 0)))
        ]
    (cond-> {}
      ms  (assoc :ms ms)
      s   (assoc :sec s)
      m   (assoc :min m)
      h   (assoc :hour h)
      d   (assoc :day d)
      mm  (assoc :month mm)
      y   (assoc :year y))))

(comment
  (rem 24 12)
  (rem 24 12)
  (quot 12 12)
  (quot 24 12)
  (plus 
   {:year 2019
    :month 12
    :day 1}
   {:month 1}
   )
 )

(def months
  {0  {:name "January" :days 31}
   1  {:name "February" :days 28 :leap 29}
   2  {:name "March" :days 31}
   3  {:name "April" :days 30}
   4  {:name "May" :days 31}
   5  {:name "June" :days 30}
   6  {:name "July" :days 31}
   7  {:name "August" :days 31}
   8  {:name "September" :days 30}
   9  {:name "October" :days 31}
   10 {:name "November" :days 30}
   11 {:name "December" :days 31}})

(def weeks
  {0 {:name "Sunday"}
   1 {:name "Monday"}
   2 {:name "Tuesday"}
   3 {:name "Wednesday"}
   4 {:name "Thursday"}
   5 {:name "Friday"}
   6 {:name "Saturday"}})

(defn number-of-days [y m]
  (cond
    (contains? #{4 6 9 11} m) 30
    (= 2 m (is-leap? y)) 29
    (= 2 m) 28
    :else 31))

(defn shift-month [y m dir]
  (let [m (+ m (if (= :next dir) 1 -1))]
    (cond
      (< m  1) [(dec y) 12]
      (> m 12) [(inc y) 1]
      :else [y m])))


(defn day-of-week
  "m 1-12; y > 1752"
  [y m d & [fmt]]
  (let [t [nil 0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4]
        y (- y (if (< m 3) 1 0))
        dow (rem (+ y
                    (int (/ y 4))
                    (- (int (/ y 100)))
                    (int (/ y 400))
                    (nth t m)
                    d) 7)]

    (if (= :ru fmt)
      (let [dow (- dow 1)]
        (if (< dow 0) 6 dow))
      dow)))


;; TODO: use memoize
(comment
  (shift-month 2019 12 :next)
  (shift-month 2020 1 :prev)
  (clojure.pprint/pprint (for-month 2020 1 :ru))
  )

(defn for-month [y m & [fmt {today :today active :active}]]
  (let [start-day (day-of-week y m 1 fmt)
        start-month (if (= 1 m) 12 (dec m))
        pm-num-days (number-of-days (if (= 1 m) (dec y) y) start-month)
        pm-last-day {:month start-month :day pm-num-days}
        start-cal (if (= 0 start-day)
                    {:month m :day 1}
                    {:month start-month :day (inc (- pm-num-days start-day))})
        num-d     (number-of-days y m)]
    {:year y :month m
     :cal (for [r (range 6)]
            (for [wd (range 7)]
              (let [idx (+ (* r 7) wd)
                    d (inc (- idx start-day))
                    cell (cond
                           (< idx start-day)
                           {:year (if (= 1 m) (dec y) y) :month start-month :day (+ (:day start-cal) idx)}

                           (> d num-d)
                           {:year (if (= m 12) (inc y) y) :month (if (= 12 m) 1 (inc m)) :day (inc (- idx start-day num-d))}

                           :else
                           {:year y :month m :day d :current true})]
                (if (and (= (:year cell) (:year active))
                         (= (:month cell) (:month active))
                         (= (:day cell) (:day active)))
                  (assoc cell :active true)
                  cell))))}))

(def month-names
  {1  {:name "January" :short "Jan"}
   2  {:name "February" :short "Feb"}
   3  {:name "March" :short "Mar"}
   4  {:name "April" :short "Apr"}
   5  {:name "May" :short "May"}
   6  {:name "June" :short "June"}
   7  {:name "July" :short "July"}
   8  {:name "August" :short "Aug"}
   9  {:name "September" :short "Sep"}
   10 {:name  "October" :short "Oct"}
   11 {:name  "November" :short "Nov"}
   12 {:name  "December" :short "Dec"}})


(def month-names-ru
  {1  {:name "Январь"   :short "..."}
   2  {:name "Февраль"  }
   3  {:name "Март"     }
   4  {:name "Апрель"   }
   5  {:name "Май"      }
   6  {:name "Июнь"     }
   7  {:name "Июль"     }
   8  {:name "Август"   }
   9  {:name "Сентябрь" }
   10 {:name "Октябрь"  }
   11 {:name "Ноябрь"   }
   12 {:name "Декабрь"  }})
