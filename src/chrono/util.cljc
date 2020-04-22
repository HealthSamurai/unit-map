(ns chrono.util
  (:require [clojure.string :as str]))

(defmulti locale (fn[x] x))

(def parse-patterns
  {:year  "(?:\\d\\d\\d\\d|\\d\\d\\d|\\d\\d|\\d)"
   :month "(?:1[0-2]|0[1-9]|[1-9]|\\p{L}+\\.?)"
   :day   "(?:3[0-1]|[1-2]\\d|0[1-9]|[1-9])"
   :hour  "(?:2[0-3]|[0-1]\\d|\\d)"
   :min   "(?:[0-5]\\d|\\d)"
   :sec   "(?:[0-5]\\d|\\d)"
   :ms    "(?:\\d\\d\\d|\\d\\d|\\d)"})

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

(def iso-fmt [:year "-" :month "-" :day "T" :hour ":" :min ":" :sec "." :ms])

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

(defn days-in-month [{m :month, y :year}]
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
