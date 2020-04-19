(ns chrono.io
  (:refer-clojure :exclude [format])
  #?@
   (:clj
    [(:require
      [chrono.ops :as ops]
      [chrono.util :as util]
      [clojure.string :as str])]
    :cljs
    [(:require
      [chrono.ops :as ops]
      [chrono.util :as util]
      [clojure.string :as str]
      goog.string)]))

(defn- internal-parse [s fmt strict?]
  (letfn [(match [f s] (-> (or (util/parse-patterns f) (util/sanitize f))
                           (#(str "(" % ")" "(.+)?"))
                           re-pattern
                           (re-matches s)))
          (match-collection [process-fn {:keys [s f p] :as r}]
            (loop
                [[f & rest-f] f
                 s s
                 acc nil]
              (let [[match? s rest-s] (process-fn f s)]
                (if-not match? nil
                        (let [acc (cond-> acc (keyword? f) (assoc f s))]
                          (if (and rest-s rest-f)
                            (recur rest-f rest-s acc)
                            {:acc acc :f rest-f :s rest-s}))))))]
    (let [lang (-> fmt meta ffirst)
          res (match-collection match {:s s :f fmt})]
      (if-not (and strict? (or (some? (:s res)) (some? (:f res))))
        (some-> res
                :acc
                (#(zipmap (keys %) (map (fn [x]
                                          (util/parse-val
                                           (val x)
                                           {:unit (key x) :lang lang})) %))))))))
(defn parse
  ([s] (parse s util/iso-fmt))
  ([s fmt] (internal-parse s fmt false)))

(defn strict-parse
  ([s] (strict-parse s util/iso-fmt))
  ([s fmt] (internal-parse s fmt true)))

(defn- format-str [v [fmt & fmt-args] lang]
  (if (and (some? lang) (contains? (util/locale lang) fmt))
    (let [full? (empty? (filter #(= % :short) fmt-args))]
      (-> (util/locale lang)
          fmt
          (get-in [v (if full? :name :short)])))

    (let [width (or (first (filter integer? fmt-args)) (util/format-patterns fmt))]
      (->>
       (apply
        #?(:clj  clojure.core/format
           :cljs goog.string/format)
        (str "%0" width \d)
        [v])
       (take-last width)
       (str/join)))))

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
               (format-fn v fmt lang))))
          str/join))))

(defn date-convertable? [value in out]
  (ops/eq?
   (parse value in)
   (parse (format (parse value in) out) out)))

(defn date-valid? [value fmt]
  #?(:clj true ; TODO
     :cljs (not (js/isNaN (.parse js/Date (format (parse value fmt) [:year "-" :month "-" :day]))))))
