(ns chrono.io
  (:require [chrono.util :as util]
            [chrono.ops :as ops]
            [clojure.string :as str]
            #?(:cljs [goog.string])
            #?(:cljs [goog.string.format]))
  (:refer-clojure :exclude [format]))

(defn- format-str [v [fmt & fmt-args] lang]
  (if (and (some? lang) (contains? (util/locale lang) fmt))
    (let [full? (empty? (filter #(= % :short) fmt-args))]
      (-> (util/locale lang)
          fmt
          (get-in [v (if full? :name :short)])))

    (let [width (or (first (filter integer? fmt-args)) (util/format-patterns fmt))]
      (if width
        (->>
         (apply
          #?(:clj  clojure.core/format
             :cljs goog.string/format)
          (str "%0" width \d)
          [v])
         (take-last width)
         (str/join))
        (str v)))))

(defn- internal-parse [s fmt strict?]
  (letfn [(match [f s] (-> (or (util/parse-patterns f) (util/sanitize f))
                           (#(str "(" % ")" "(.+)?"))
                           re-pattern
                           (re-matches s)))
          (match-collection [process-fn {:keys [s f p] :as r} lang]
            (loop
                [[f & rest-f] f
                 s s
                 acc nil]
              (let [[match? s rest-s] (process-fn f s)
                    v (if (keyword? f) (util/parse-val s f lang) s)]
                (if-not (and match? v) {:acc acc :f rest-f :s rest-s}
                        (let [acc (cond-> acc (keyword? f) (assoc f v))]
                          (if (and rest-s rest-f)
                            (recur rest-f rest-s acc)
                            {:acc acc :f rest-f :s rest-s}))))))]
    (let [lang (-> fmt meta ffirst)
          res (match-collection match {:s s :f fmt} lang)]
      (if-not (and strict? (or (some? (:s res)) (some? (:f res)))) (:acc res)))))

(defn parse
  ([s] (parse s util/iso-fmt))
  ([s fmt] (internal-parse s fmt false)))

(defn strict-parse
  ([s] (strict-parse s util/iso-fmt))
  ([s fmt] (internal-parse s fmt true)))

(defn format
  ([date-coll] (format date-coll util/iso-fmt))
  ([date-coll fmt-vec]
   (let [lang (-> fmt-vec meta ffirst)]
     (->> fmt-vec
          (mapv
           (fn [fmt]
             (let [fmt (cond-> fmt (not (vector? fmt)) vector)
                   f (first fmt)
                   v (get date-coll f)
                   format-fn (if (keyword? f)
                               (or (first (filter fn? fmt)) format-str)
                               (constantly (or v f)))]
               (format-fn (or v 0) fmt lang))))
          str/join))))

(defn date-convertable? [value in out]
  (ops/eq?
   (parse value in)
   (parse (format (parse value in) out) out)))

(defn date-valid? [value fmt]
  #?(:clj true ; TODO
     :cljs (not (js/isNaN (.parse js/Date (format (parse value fmt) [:year "-" :month "-" :day]))))))
