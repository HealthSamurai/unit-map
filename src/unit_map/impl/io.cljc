(ns unit-map.impl.io
  (:refer-clojure :exclude [format])
  (:require [unit-map.impl.util :as util]
            [clojure.string :as str]))


;; TODO: move all date time related consts to type definition


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
   {1  {:full "January" :short "Jan" :regex "(?i)jan\\S*"}
    2  {:full "February" :short "Feb" :regex "(?i)feb\\S*"}
    3  {:full "March" :short "Mar" :regex "(?i)mar\\S*"}
    4  {:full "April" :short "Apr" :regex "(?i)apr\\S*"}
    5  {:full "May" :short "May" :regex "(?i)may\\S*"}
    6  {:full "June" :short "June" :regex "(?i)jun\\S*"}
    7  {:full "July" :short "July" :regex "(?i)jul\\S*"}
    8  {:full "August" :short "Aug" :regex "(?i)aug\\S*"}
    9  {:full "September" :short "Sep" :regex "(?i)sep\\S*"}
    10 {:full "October" :short "Oct" :regex "(?i)oct\\S*"}
    11 {:full "November" :short "Nov" :regex "(?i)nov\\S*"}
    12 {:full "December" :short "Dec" :regex "(?i)dec\\S*"}}})


(defmethod locale :en [_] locale-en)


(defmethod locale :default [_] locale-en)


(defmethod locale :ru [_]
  {:month
   {1 {:full "Январь", :short "Янв", :regex "(?iu)янв(ар(ь|я))?"}
    2 {:full "Февраль", :short "Фев", :regex "(?iu)фев(рал(ь|я))?"}
    3 {:full "Март", :short "Мар", :regex "(?iu)мар(та?)?"}
    4 {:full "Апрель", :short "Апр", :regex "(?iu)апр(ел(ь|я)?)?"}
    5 {:full "Май", :short "Май", :regex "(?iu)ма(й|я)?"}
    6 {:full "Июнь", :short "Июн", :regex "(?iu)июн(ь|я)?"}
    7 {:full "Июль", :short "Июл", :regex "(?iu)июл(ь|я)?"}
    8 {:full "Август", :short "Авг", :regex "(?iu)авг(уста?)?"}
    9 {:full "Сентябрь", :short "Сен", :regex "(?iu)сен(тябр(ь|я)?)?"}
    10 {:full "Октябрь", :short "Окт", :regex "(?iu)окт(ябр(ь|я)?)?"}
    11 {:full "Ноябрь", :short "Ноя", :regex "(?iu)ноя(бр(ь|я)?)?"}
    12 {:full "Декабрь", :short "Дек", :regex "(?iu)дек(бр(ь|я)?)?"}}})


(defn parse-name [name unit lang]
  (when name
    (-> (locale lang)
        (get unit)
        (->> (filter #(re-matches (-> % val :regex re-pattern) name)))
        ffirst)))


(defn parse-val [fmt-el x]
  (or (util/try-parse-long x)
      (parse-name x (:value fmt-el) (:lang fmt-el))))


(defn get-lang [fmt-vec fmt-el]
  (ffirst (meta fmt-el)))


(defn el->regex [{:keys [value width]}]
  (cond
    (util/regex? value) value
    (some? width)       (apply str (repeat width \.))
    (keyword? value)    ".+?"
    :else               (util/sanitize value)))


(defn read-fmt-el [fmt-vec fmt-el] ;; TODO: maybe make this as date-reader for fmt-vec?
  (let [[value & rest-fmt] (flatten (vector fmt-el))
        lang               (get-lang fmt-vec fmt-el)
        width              (util/ffilter integer? rest-fmt)]
    {:value     value
     :lang      lang
     :name-fmt  (or (util/ffilter keyword? rest-fmt)
                    (when lang :full))
     :function  (util/ffilter fn? rest-fmt)
     :pad-width width
     :pad-str   (util/ffilter (some-fn string? char?) rest-fmt)
     :regex     (or (and (keyword? value)
                         (util/ffilter util/regex? rest-fmt))
                    (el->regex {:value value, :width width}))}))


(defn mk-group-regex [cur-group next-group]
  (let [{el-regex :regex, :as el} (last cur-group)
        cur-group-border  (map :regex (butlast cur-group))
        next-group-border (:regex (first next-group))
        group-regex       (str \("?:"
                               (str/join cur-group-border)
                               \( el-regex \)
                               \( "?=" \( "?:" next-group-border \| "$" \) \) \))]
    (assoc el :group-regex group-regex)))


(defn make-regex-groups [fmt-vec]
  (transduce
    (map (partial read-fmt-el fmt-vec))
    (fn
      ([acc el]
       (if (keyword? (:value el))
         {:group []
          :result (conj (:result acc) (conj (:group acc) el))}
         (update acc :group conj el)))
      ([acc]
       (let [result (conj (:result acc) (:group acc))]
         (mapv mk-group-regex result (rest result)))))
    {:group []
     :result []}
    (concat [#"^"] fmt-vec [#"$"])))


(defn parse-groups [acc s [el & rest-els] & {:keys [strict]}]
  (cond
    (not (str/blank? s))
    (let [pat                    (re-pattern (str (:group-regex el) "(.*$)?"))
          [match-s cur-s rest-s] (re-find pat s)
          found?                 (not (str/blank? match-s))
          parsed-value           (when found? (parse-val el cur-s))]
      (when (or (not strict) found?)
        (recur (cond-> acc
                 parsed-value
                 (assoc (:value el) parsed-value))
               rest-s
               rest-els
               {:strict strict})))

    (or (not strict) (empty? el))
    acc))


(defn parse [s fmt-vec & {:keys [strict]}]
  (parse-groups {} s (make-regex-groups fmt-vec) :strict strict))


(defn format-el [value fmt-vec fmt-el]
  (let [{:keys [lang function name-fmt pad-width pad-str], fmt :value :as fmt-el} (read-fmt-el fmt-vec fmt-el)

        v          (get value fmt)
        unit-value (or (when function (function value fmt-el))
                       (when lang (get-in (locale lang) [fmt v name-fmt]))
                       v
                       fmt)
        pad-width  (or pad-width (max (format-patterns fmt 0) (count (str unit-value))))
        pad-str    (or pad-str
                       (when (number? unit-value) 0)
                       " ")]
    (cond->> (str unit-value)
      (not (zero? pad-width))
      (util/pad-str pad-str pad-width))))


(defn format [t fmt-vec]
  (str/join (map (partial format-el t fmt-vec) fmt-vec)))


#_(defn convertable? [value in out]
    (let [v (parse value in)]
      (ops/eq? v (-> v  (format out) (parse out)))))


#_(defn valid? [s fmt]
    (let [d (parse s fmt)]
      (ops/eq? d (ops/ensure-less-significant-units d))))
