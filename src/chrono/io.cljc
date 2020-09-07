(ns chrono.io
  (:require [chrono.util :as u]
            [chrono.new-ops :as new-ops]
            [clojure.string :as str])
  (:refer-clojure :exclude [format]))


(def parse-patterns
  {:year  "(?:\\d\\d\\d\\d|\\d\\d\\d|\\d\\d|\\d)"
   :month #?(:clj  "(?:1[0-2]|0[1-9]|[1-9]|\\p{L}+\\.?)"
             :cljs "(?:1[0-2]|0[1-9]|[1-9]|\\w+\\.?)") ;; TODO: can't get \p{L} work in cljs
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


(defmulti locale (fn[x] x))


(def locale-en
  {:month
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


(defn parse-name [name unit lang]
  (when name
    (-> (locale lang)
        (get unit)
        (->> (filter #(re-matches (-> % val :regex re-pattern) name)))
        ffirst)))


(defn parse-val [x unit lang]
  (or (u/parse-int x)
      (parse-name x unit lang)))


(defmethod locale :ru [_]
  {:month
   {1 {:name "Январь", :short "Янв", :regex "(?iu)янв(ар(ь|я))?"}
    2 {:name "Февраль", :short "Фев", :regex "(?iu)фев(рал(ь|я))?"}
    3 {:name "Март", :short "Мар", :regex "(?iu)мар(та?)?"}
    4 {:name "Апрель", :short "Апр", :regex "(?iu)апр(ел(ь|я)?)?"}
    5 {:name "Май", :short "Май", :regex "(?iu)ма(й|я)?"}
    6 {:name "Июнь", :short "Июн", :regex "(?iu)июн(ь|я)?"}
    7 {:name "Июль", :short "Июл", :regex "(?iu)июл(ь|я)?"}
    8 {:name "Август", :short "Авг", :regex "(?iu)авг(уста?)?"}
    9 {:name "Сентябрь", :short "Сен", :regex "(?iu)сен(тябр(ь|я)?)?"}
    10 {:name "Октябрь", :short "Окт", :regex "(?iu)окт(ябр(ь|я)?)?"}
    11 {:name "Ноябрь", :short "Ноя", :regex "(?iu)ноя(бр(ь|я)?)?"}
    12 {:name "Декабрь", :short "Дек", :regex "(?iu)дек(бр(ь|я)?)?"}}})


(defn parse [s fmt & {:keys [strict], :or {strict false}}]
  (let [fmts (map #(cond-> % (vector? %) first) fmt)
        pat  (map (some-fn parse-patterns u/sanitize) fmts)]
    (loop [s            s
           [f & rest-f] fmts
           [p & rest-p] pat
           acc          (with-meta {} (meta fmt))]
      (if-not (and s f)
        acc
        (let [pat (->> (if strict
                         (str \( (apply str rest-p) \))
                         "(.+)?")
                       (str "(" p ")")
                       re-pattern)

              [match-s cur-s rest-s] (re-matches pat s)]
          (cond
            match-s (recur rest-s rest-f rest-p
                           (cond-> acc
                             (contains? parse-patterns f)
                             (assoc f (u/parse-int cur-s))))

            (not strict) acc))))))


(defn format
  ([t fmt-vec]
   (letfn [(format-el [value fmt-el]
             (let [[fmt pad-width pad-str] (flatten (vector fmt-el))

                   unit-value (or (get value fmt)
                                  (new-ops/sequence-nth (new-ops/unit-definition value fmt) value 0)
                                  fmt)
                   pad-width  (or pad-width (format-patterns fmt) (count (str unit-value)))
                   pad-str    (or pad-str
                                  (when (number? unit-value) 0)
                                  " ")]
               (u/pad-str pad-str pad-width (str unit-value))))]
     (str/join (map (partial format-el t) fmt-vec)))))


(defn date-convertable? [value in out]
  (let [v (parse value in)]
    (new-ops/eq? v (-> v  (format out) (parse out)))))


(defn date-valid? [s fmt]
  (let [d (parse s fmt)]
    (new-ops/eq? d (new-ops/ensure-less-significant-units d))))
