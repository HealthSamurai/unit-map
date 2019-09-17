(ns chrono.core
  (:require [chrono.util :as util]
            [clojure.string :as str]
            #?(:cljs [goog.string])
            #?(:cljs [goog.string.format])))

(def sprintf #?(:clj  clojure.core/format, :cljs goog.string/format))

(defn datetime [t]
  (merge {:type :datetime
          :year 1900
          :month 1
          :day 1} t))

(defn now []
  (let [now   (#?(:clj  java.time.LocalDateTime/now
                  :cljs js/Date.))
        year  #?(:clj  (-> now .getYear)
                 :cljs (-> now .getFullYear))
        month #?(:clj  (-> now .getMonthValue)
                 :cljs (-> now .getMonth inc))
        day   #?(:clj  (-> now .getDayOfMonth)
                 :cljs (-> now .getDate))
        hour  #?(:clj  (-> now .getHour)
                 :cljs (-> now .getHours))
        min   #?(:clj  (-> now .getMinute)
                 :cljs (-> now .getMinutes))
        sec   #?(:clj  (-> now .getSecond)
                 :cljs (-> now .getSeconds))
        ms    #?(:clj  (-> now .getNano (/ 1000000))
                 :cljs (-> now .getMilliseconds))]
    {:year  year
     :month month
     :day   day
     :hour  hour
     :min   min
     :sec   sec
     :ms    ms}))

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

(defn- safe+ [& args]
  (when (some identity args)
    (reduce + (map #(or % 0) args))))

(def ^{:private true} quot-rem (juxt quot rem))

(defn plus
  [{ms' :ms s' :sec m' :min h' :hour d' :day mm' :month y' :year :as t}
   {ms'' :ms s'' :sec m'' :min h'' :hour d'' :day mm'' :month y'' :year :as i}]
  (let [[s+ ms] (some-> (safe+ ms' ms'') (quot-rem 1000))
        [m+ s]  (some-> (safe+ s' s'' s+) (quot-rem 60))
        [h+ m]  (some-> (safe+ m' m'' m+) (quot-rem 60))
        [d+ h]  (some-> (safe+ h' h'' h+) (quot-rem 24))
        d (safe+ d' d'' d+)
        [y' mm' d] (if (and d y' mm')
                     (days-and-months y' mm' d)
                     [y' mm' d])
        [y+ mm] (some-> (safe+ mm' mm'') (quot-rem 12))
        y (safe+ y' y'' y+)]
    (cond-> {}
      ms  (assoc :ms ms)
      s   (assoc :sec s)
      m   (assoc :min m)
      h   (assoc :hour h)
      d   (assoc :day d)
      mm  (assoc :month mm)
      y   (assoc :year y))))

(defn day-of-week
  "m 1-12; y > 1752"
  [y m d]
  (let [t [nil 0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4]
        y (- y (if (< m 3) 1 0))]
    (rem (+ y
            (int (/ y 4))
            (- (int (/ y 100)))
            (int (/ y 400))
            (nth t m)
            d) 7)))

(defmulti day-saving "[tz y]" (fn [tz _] tz))

;; rules from tzdb like sun >= 8
(defn *more-or-eq [y m dw d]
  (let [dw' (day-of-week y m d)]
    (cond (= dw' dw) d
          ;; if wed vs sun
          (> dw' dw) (+ d (- 7 dw') dw)
          (< dw' dw) (+ d (- dw dw')))))

(def more-or-eq (memoize *more-or-eq))

(defmethod day-saving
  :ny
  [_ y]
  (assert (> y 2006) "Not impl.")
  {:offset 5
   :ds -1
   :in {:year y :month 3 :day (more-or-eq y 3 0 8) :hour 2 :min 0}
   :out {:year y :month 11 :day (more-or-eq y 11 0 1) :hour 2 :min 0}})

(defn *day-saving-with-utc [tz y]
  (let [ds (day-saving tz y)]
    (assoc ds
           :in-utc (plus (:in ds) {:hour (:offset ds)})
           :out-utc (plus (:out ds) {:hour (+ (:offset ds) (:ds ds))}))))

(def day-saving-with-utc (memoize *day-saving-with-utc))


(def default-time {:year 0 :month 1 :day 1 :hour 0 :min 0 :sec 0})
(def defaults-units  [[:year 0] [:month 1] [:day 1] [:hour 0] [:min 0] [:sec 0]])

(defn after? [t t']
  (loop [[[p s] & ps] defaults-units]
    (let [tp (get t p s) tp' (get t' p s)]
      (cond
        (> tp tp') true
        (= tp tp') (and (not (empty? ps)) (recur ps))
        :else false))))

(defn eq? [t t']
  (let [t (merge default-time t)
        t' (merge default-time t')]
    (and
     (= (:year t) (:year t'))
     (= (:month t) (:month t'))
     (= (:day t) (:day t'))
     (= (:hour t) (:hour t'))
     (= (:min t) (:min t'))
     (= (:sec t) (:sec t')))))

(defn before=? [t t']
  (not (after? t t')))

(defn from-utc [t tz])

(defn parse-int [x]
  (when (string? x)
    #?(:clj (Integer/parseInt x)
       :cljs (js/parseInt  x))))

(def iso-fmt [:year "-" :month "-" :day "T" :hour ":" :min ":" :sec "." :ms])

(def parse-patterns
  {:year  "\\d{1,4}"
   :month "[01]?\\d"
   :day   "[0-3]?\\d"
   :hour  "(?:2[0-4]|[0-1]?\\d)"
   :min   "[0-5]?\\d"
   :sec   "[0-5]?\\d"
   :ms    "\\d{1,3}"})

(def format-patterns
  {:year  4
   :month 2
   :day   2
   :hour  2
   :min   2
   :sec   2
   :ms    3})

(defn sanitize [s]
  (str/replace s #"[-.\+*?\[^\]$(){}=!<>|:\\]" #(str \\ %)))

(defn parse
  ([s] (parse s iso-fmt))
  ([s fmt]
   (let [fmt (map #(cond-> % (vector? %) first) fmt)
         pat (map #(or (parse-patterns %) (sanitize %)) fmt)]
     (loop [s            s
            [f & rest-f] fmt
            [p & rest-p] pat
            acc          {}]
       (if-not (and s f)
         acc
         (let [ahead            "(.+)?"
               pat              (re-pattern (str "(" p ")" ahead))
               [_ cur-s rest-s] (re-matches pat s)]
           (recur rest-s rest-f rest-p
                  (cond-> acc
                    (contains? parse-patterns f)
                    (assoc f (parse-int cur-s))))))))))

(defn format
  ([t] (format t iso-fmt))
  ([t fmt-vec]
   (->> fmt-vec
        (mapv (fn form [x]
                (let [kw (cond-> x (vector? x) first)
                      v  (get t kw 0)]
                  (if (contains? format-patterns kw)
                    (sprintf (str "%0" (if (vector? x) (second x) (format-patterns x)) \d) v)
                    x))))
        str/join)))

(defn timestamp [t])

(defn diff [t t'])
