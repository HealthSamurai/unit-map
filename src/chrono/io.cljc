(ns chrono.io
  (:require [chrono.util :as util]
            [chrono.ops :as ops]
            [chrono.new-ops :as new-ops]
            [clojure.string :as str])
  (:refer-clojure :exclude [format]))


(defn- format-str [v [fmt & fmt-args] lang]
  (if (and lang (contains? (util/locale lang) fmt))
    (let [short? (not (empty? (filter #(= % :short) fmt-args)))]
      (-> (util/locale lang)
          fmt
          (get-in [v (if short? :short :name)])))

    (let [width (or (first (filter integer? fmt-args)) (util/format-patterns fmt))]
      (cond->> (str v)
        width (util/pad-zero width)))))


(defn- internal-parse [s fmt strict?]
  (letfn [(match [f s] (-> (or (util/parse-patterns f) (util/sanitize f))
                           (as-> $ (str "(" $ ")" "(.+)?"))
                           re-pattern
                           (re-matches s)))
          (match-collection [process s f lang]
            (loop
                [[f & rest-f] f
                 s s
                 acc nil]
              (let [unit? (keyword? f)
                    [match? s rest-s] (process f s)
                    parsed-value (if unit? (util/parse-val s f lang))
                    parsed? (or parsed-value (not unit?))
                    acc (cond-> acc parsed-value (assoc f parsed-value))]
                (if (and parsed? (some? rest-s) (some? rest-f))
                  (recur rest-f rest-s acc)
                  [acc rest-f rest-s]))))]
    (let [lang (-> fmt meta ffirst)
          [acc rest-f rest-s] (match-collection match s fmt lang)]
      (when (or (not strict?)
                (and (nil? rest-s)
                     (nil? rest-f)))
        acc))))


(defn parse
  ([s] (parse s util/iso-fmt))
  ([s fmt]
   (when-not (str/blank? s)
     (internal-parse s fmt false))))


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
   (-> value (parse in) (format out) (parse out))))


(defn date-valid? [s fmt]
  (let [d (parse s fmt)]
    (new-ops/eq? d (new-ops/ensure-less-significant-units d))))
