(ns chrono.core
  (:require [chrono.util :as util]))

(defn datetime [t]
  (merge {:type :datetime
          :year 1900
          :month 1
          :day 1} t))

(def month-length
  {1 31
   2 :leap
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

        [d mm+] (when d
                  (cond
                    (< 0 d 28) [d nil]
                    (< 28 d 59)
                    (let [len (if (= 2 mm')
                                (if (is-leap? y') 29 28)
                                (get month-length mm'))]
                      (cond
                        (<= d len) [d nil] 
                        (> d len) [(- d len) 1] 
                        :else (assert false)))

                    :else (assert false "Not impl")))

        mm (when (or mm' mm'' mm+) (+ (or mm' 0) (or mm'' 0) (or mm+ 0)))

        [mm y+] (when mm [(rem mm 12) (quot mm 12)])

        y (when (or y' y'' y+) (+ (or y' 0) (or y'' 0) (or y+ 0)))]
    (cond-> {}
      ms (assoc :ms ms)
      s (assoc :sec s)
      m (assoc :min m)
      h (assoc :hour h)
      d (assoc :day d)
      mm (assoc :month mm)
      y (assoc :year y))))


(defn parse [s format])

(defn format [t format])

(defn timestamp [t])

(defn diff [t t'])

(defn to-tz [t tz])
