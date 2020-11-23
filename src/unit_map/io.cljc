(ns unit-map.io
  (:require [unit-map.util :as u]
            [unit-map.ops :as ops]
            [clojure.string :as str])
  (:refer-clojure :exclude [format]))


;; TODO: move all date time related consts to type definition

(defmulti field-settings (fn [x] x))


(defmethod field-settings :default [_] {})


(defmulti field-locale (fn [x y] [x y]))


(defmethod field-locale :default [_ _] {})


(defn fs-default-pad-width [field]
  (if (keyword? field)
    (get (field-settings field) :default-pad-width 0)
    0))


(defn parse-name [name unit lang]
  (when name
    (-> (field-locale unit (or lang :default))
        (->> (filter #(re-matches (-> % val :regex re-pattern) name)))
        ffirst)))


(defn parse-val [fmt-el x]
  (or (parse-name x (:value fmt-el) (:lang fmt-el))
      (u/try-parse-int x)))


(defn get-lang [fmt-vec fmt-el]
  (ffirst (meta fmt-el)))


(defn el->regex [{:keys [value]}]
  (cond
    (u/regex? value)   value
    (keyword? value) ".+?"
    :else            (u/sanitize value)))


(defn read-fmt-el [fmt-vec fmt-el] ;; TODO: maybe make this as date-reader for fmt-vec?
  (let [[value & rest-fmt] (flatten (vector fmt-el))
        lang               (get-lang fmt-vec fmt-el)]
    {:value     value
     :lang      lang
     :type      (ops/get-type fmt-vec)
     :name-fmt  (or (u/ffilter keyword? rest-fmt)
                    (when lang :full))
     :function  (u/ffilter fn? rest-fmt)
     :pad-width (u/ffilter integer? rest-fmt)
     :pad-str   (u/ffilter (some-fn string? char?) rest-fmt)
     :regex     (el->regex {:value value})}))


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
  (as-> fmt-vec $
    (concat [#"^"] $ [#"$"])
    (map (partial read-fmt-el fmt-vec) $)
    (partition-by (comp keyword? :value) $)
    (partition 2 2 [] $)
    (map (partial apply concat) $)
    (map mk-group-regex $ (rest $))))


(defn parse [s fmt-vec & {:keys [strict], :or {strict false}}]
  (loop [s               s
         [el & rest-els] (make-regex-groups fmt-vec)
         acc             (with-meta {} (meta fmt-vec))]
    (cond
      (not (str/blank? s))
      (let [pat                    (re-pattern (str (:group-regex el) "(.*$)?"))
            [match-s cur-s rest-s] (re-find pat s)
            found?                 (not (str/blank? match-s))
            parsed-value           (when found? (parse-val el cur-s))]
        (when (or (not strict) found?)
          (recur rest-s
                 rest-els
                 (cond-> acc parsed-value (assoc (:value el) parsed-value)))))

      (or (not strict) (empty? el))
      acc)))


(defn format-el [value fmt-vec fmt-el]
  (let [{:keys [lang function name-fmt pad-width pad-str], fmt :value :as fmt-el} (read-fmt-el fmt-vec fmt-el)

        v          (get value fmt)
        unit-value (or (when function (function value fmt-el))
                       (when lang (get-in (field-locale fmt (or lang :default)) [v name-fmt]))
                       v
                       (ops/sequence-nth (ops/unit-definition value fmt) value 0)
                       fmt)
        pad-width  (or pad-width (max (fs-default-pad-width fmt) (count (str unit-value))))
        pad-str    (or pad-str
                       (when (number? unit-value) 0)
                       " ")]
    (cond->> (str unit-value)
      (not (zero? pad-width))
      (u/pad-str pad-str pad-width))))


(defn format [t fmt-vec]
  (str/join (map (partial format-el t fmt-vec) fmt-vec)))


(defn convertable? [value in out]
  (let [v (parse value in)]
    (ops/eq? v (-> v  (format out) (parse out)))))


(defn valid? [s fmt]
  (let [d (parse s fmt)]
    (ops/eq? d (ops/ensure-less-significant-units d))))
