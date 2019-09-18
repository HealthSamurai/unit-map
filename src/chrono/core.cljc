(ns chrono.core
  (:require [chrono.util :as util]
            [chrono.tz :as tz]
            [chrono.calendar :as cal]
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

(defn- safe+ [& args]
  (when (some identity args)
    (reduce + (map #(or % 0) args))))

(def ^{:private true} quot-rem (juxt quot rem))

(defn- plus
  [{ms' :ms s' :sec m' :min h' :hour d' :day mm' :month y' :year :as t}
   {ms'' :ms s'' :sec m'' :min h'' :hour d'' :day mm'' :month y'' :year :as i}]
  (let [[s+ ms] (some-> (safe+ ms' ms'') (quot-rem 1000))
        [m+ s]  (some-> (safe+ s' s'' s+) (quot-rem 60))
        [h+ m]  (some-> (safe+ m' m'' m+) (quot-rem 60))
        [d+ h]  (some-> (safe+ h' h'' h+) (quot-rem 24))
        y'mm'd  [y' mm' (safe+ d' d'' d+)]
        [y' mm' d] (cond->> y'mm'd (every? identity y'mm'd) (apply tz/days-and-months))
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
  ([s] (parse s util/iso-fmt))
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
  ([t] (format t util/iso-fmt))
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

(def + plus)
