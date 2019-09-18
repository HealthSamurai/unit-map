(ns chrono.core
  (:require [chrono.util :as util]
            [chrono.tz :as tz]
            [chrono.now :as now]
            [clojure.string :as str]))

(defn datetime [t]
  (merge {:type :datetime
          :year 1900
          :month 1
          :day 1} t))

(def ^{:private true} parse-patterns
  {:year  "\\d{1,4}"
   :month "[01]?\\d"
   :day   "[0-3]?\\d"
   :hour  "(?:2[0-4]|[0-1]?\\d)"
   :min   "[0-5]?\\d"
   :sec   "[0-5]?\\d"
   :ms    "\\d{1,3}"})

(def ^{:private true} format-patterns
  {:year  4
   :month 2
   :day   2
   :hour  2
   :min   2
   :sec   2
   :ms    3})

(defn- sanitize [s]
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
                    (assoc f (util/parse-int cur-s))))))))))

(defn format
  ([t] (format t util/iso-fmt))
  ([t fmt-vec]
   (->> fmt-vec
        (mapv (fn form [x]
                (let [kw (cond-> x (vector? x) first)
                      v  (get t kw 0)]
                  (if (contains? format-patterns kw)
                    (util/format (str "%0" (if (vector? x) (second x) (format-patterns x)) \d) v)
                    x))))
        str/join)))

(defn timestamp [t])

(defn diff [t t'])

(def normalize tz/normalize)

(def + tz/+)
(def = tz/=)
(def > tz/>)
(def >= tz/>=)
(def < tz/<)
(def <= tz/<=)
