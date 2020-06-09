(ns chrono.util
  (:require [clojure.string :as str]
            [chrono.datetime :as cd]
            [chrono.interval :as ci]))

(defn group-keys [m]
  (reduce-kv (fn [result k v]
               (let [key-namespace (-> k namespace keyword)
                     normalized-key (-> k name keyword)]
                 (assoc-in result
                           [key-namespace normalized-key]
                           v)))
             {}
             m))
             
(defmulti locale (fn[x] x))

(def locale-en
  {::cd/month
   {1  {:name "January" :short "Jan" :regex "(?i)jan\\S*"}
    2  {:name "February" :short "Feb" :regex "(?i)feb\\S*"}
    3  {:name "March" :short "Mar" :regex "(?i)mar\\S*"}
    4  {:name "April" :short "Apr" :regex "(?i)apr\\S*"}
    5  {:name "May" :short "May" :regex "(?i)may\\S*"}
    6  {:name "June" :short "June" :regex "(?i)jun\\S*"}
    7  {:name "July" :short "July" :regex "(?i)jul\\S*"}
    8  {:name "August" :short "Aug" :regex "(?i)aug\\S*"}
    9  {:name "September" :short "Sep" :regex "(?i)sep\\S*"}
    10 {:name "October" :short "Oct" :regex "(?i)oct\\S*"}
    11 {:name "November" :short "Nov" :regex "(?i)nov\\S*"}
    12 {:name "December" :short "Dec" :regex "(?i)dec\\S*"}}})

(defmethod locale :en [_] locale-en)
(defmethod locale :default [_] locale-en)

(def parse-patterns
  {::cd/year  "(?:\\d\\d\\d\\d|\\d\\d\\d|\\d\\d|\\d)"
   ::cd/month "(?:1[0-2]|0[1-9]|[1-9]|\\p{L}+\\.?)"
   ::cd/day   "(?:3[0-1]|[1-2]\\d|0[1-9]|[1-9])"
   ::cd/hour  "(?:2[0-3]|[0-1]\\d|\\d)"
   ::cd/min   "(?:[0-5]\\d|\\d)"
   ::cd/sec   "(?:[0-5]\\d|\\d)"
   ::cd/ms    "(?:\\d\\d\\d|\\d\\d|\\d)"
   ::ci/day   "(?:[0-9]+)"
   ::ci/hour  "(?:2[0-3]|[0-1]\\d|\\d)"
   ::ci/min   "(?:[0-5]\\d|\\d)"
   ::ci/sec   "(?:[0-5]\\d|\\d)"
   ::ci/ms    "(?:\\d\\d\\d|\\d\\d|\\d)"})

(def format-patterns
  #::cd{:year  4
        :month 2
        :day   2
        :hour  2
        :min   2
        :sec   2
        :ms    3})

(defn sanitize [s]
  (str/replace s #"[-.\+*?\[^\]$(){}=!<>|:\\]" #(str \\ %)))

(def iso-fmt [::cd/year "-" ::cd/month "-" ::cd/day "T"
              ::cd/hour ":" ::cd/min ":" ::cd/sec "." ::cd/ms])

(defn parse-name [name unit lang]
  (-> (locale lang)
      (get unit)
      (->> (filter #(re-matches (-> % val :regex re-pattern) name)))
      doall
      ffirst))

(defn parse-int [x]
  (when (string? x)
    #?(:clj (try (Integer/parseInt x) (catch NumberFormatException e nil))
       :cljs (js/parseInt x))))

(defn parse-val [x unit lang]
  (or (parse-int x)
      (parse-name x unit lang)))

(defn leap-year? [y]
  (and (zero? (rem y 4))
       (or (pos? (rem y 100))
           (zero? (rem y 400)))))

(defn days-in-month [{m ::cd/month, y ::cd/year}]
  (cond
    (contains? #{4 6 9 11} m) 30
    (and (leap-year? y) (= 2 m)) 29
    (= 2 m) 28
    :else 31))

(defn- simplify
  ([key acc] (simplify key nil acc))
  ([key max [t r]]
   (let [v (get t key)]
     (vector
      (assoc t key (if v
                     (if max
                       (rem (+ r v) max)
                       (+ r v))
                     r))
      (if (and v max)
        (quot (+ r v) max)
        0)))))

(defn- add-days [t r]
  (update t :day #(+ r (or % 1))))

(defn- simplify-month [f]
  (-> f
      (update :month #(rem % 12))
      (update :year #(+ % (quot (:month f) 12)))))

(defn- simplify-day [f]
  (-> f
      (update :day #(- % (days-in-month f)))
      (update :month inc)))

(defn- simplify-date [f]
  (cond (< 12 (:month f))
        (simplify-date (simplify-month f))
        (< (days-in-month f) (:day f))
        (simplify-date (simplify-day f))
        :else f))

(defn normalize [t]
  (case (:type t)
    :datetime (->> [t 0]
                   (simplify :second 60)
                   (simplify :minute 60)
                   (simplify :hour 24)
                   (apply add-days)
                   (simplify-date)
                   )
    :time (->> [t 0]
               (simplify :second 60)
               (simplify :minute 60)
               (simplify :hour)
               first)
    t))

(defn pad-str [p n s]
  (->> (concat (reverse s) (repeat p))
       (take n)
       reverse
       str/join))

(def pad-zero (partial pad-str \0))

(defn seconds [d]
  (+ (* (dec (::cd/day d)) 60 60 24)
     (* (::cd/hour d) 60 60)
     (* (::cd/min d) 60)
     (::cd/sec d)))

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

(defn *more-or-eq [y m dw d]
  (let [dw' (day-of-week y m d)]
    (cond (= dw' dw) d
          ;; if wed vs sun
          (> dw' dw) (+ d (- 7 dw') dw)
          (< dw' dw) (+ d (- dw dw')))))

(def more-or-eq (memoize *more-or-eq))
