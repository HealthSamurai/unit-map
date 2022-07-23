(ns unit-map.impl.io
  (:refer-clojure :exclude [format])
  (:require [unit-map.impl.util :as util]
            [clojure.string :as str]))


;; TODO: move all date time related consts to type definition
;; TODO: add convert from/to java/js date objects


(def format-patterns
  {:year  4
   :month 2
   :day   2
   :hour  2
   :min   2
   :sec   2
   :ms    3})


(defn parse-val [fmt-el x]
  (util/try-parse-long x))


(defn el->regex [{:keys [value width]}]
  (cond
    (util/regex? value) value
    (some? width)       (apply str (repeat width \.))
    (keyword? value)    ".+?"
    :else               (util/sanitize value)))


#_"TODO: maybe make this as data-reader for fmt-vec?"
(defn read-fmt-el [_fmt-vec fmt-el]
  (let [[value & rest-fmt] (flatten (vector fmt-el))
        width              (util/ffilter integer? rest-fmt)]
    {:value     value
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
        group-regex       (str "(?:" (str/join cur-group-border)
                               "(" el-regex ")"
                               "(?=(?:" next-group-border "|$)))")]

    (assoc el :group-regex group-regex)))


(defn make-regex-groups [fmt-vec]
  (let [acc (reduce (fn [acc fmt-el]
                      (let [el (read-fmt-el fmt-vec fmt-el)]
                        (if (keyword? (:value el))
                          {:group []
                           :result (-> (:result acc)
                                       (conj (conj (:group acc) el)))}
                          (update acc :group conj el))))
                    {:group [], :result []}
                    (concat [#"^"] fmt-vec [#"$"]))

        result (conj (:result acc) (:group acc))]

    (mapv mk-group-regex result (rest result))))


(defn parse-groups [acc s [el & rest-els] & {:keys [strict]}]
  (cond
    (not (str/blank? s))
    (let [pat (re-pattern (str (:group-regex el) "(.*$)?"))

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
  (let [{:as   fmt-el
         :keys [function name-fmt pad-width pad-str]
         fmt   :value}
        (read-fmt-el fmt-vec fmt-el)

        v (get value fmt)

        unit-value (or (when function (function value fmt-el))
                       v
                       fmt)

        pad-width (or pad-width
                       (max (format-patterns fmt 0)
                            (count (str unit-value))))

        pad-str (or pad-str
                       (when (number? unit-value) 0)
                       " ")]

    (cond->> (str unit-value)
      (not (zero? pad-width))
      (util/pad-str pad-str pad-width))))


(defn format [t fmt-vec]
  (->> fmt-vec
       (map #(format-el t fmt-vec %))
       str/join))


#_(defn convertable? [value in out]
    (let [v (parse value in)]
      (ops/eq? v (-> v  (format out) (parse out)))))


#_(defn valid? [s fmt]
    (let [d (parse s fmt)]
      (ops/eq? d (ops/ensure-less-significant-units d))))
