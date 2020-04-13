(ns chrono.io
  (:require [chrono.util :as util]
            [chrono.ops :as ops]
            [clojure.string :as str]
            #?(:cljs [goog.string])
            #?(:cljs [goog.string.format]))
  (:refer-clojure :exclude [format]))

(defn- format-str [fmt val width]
  (->>
    (apply
      #?(:clj  clojure.core/format
         :cljs goog.string/format)
      fmt
      [val])
    (take-last width)
    (str/join)))

(defn pv [{:keys [s acc strict?] :as res} f]
  (let [f (cond-> f (vector? f) first)
        patternize (fn [x] (str "(" x ")" "(.+)?"))
        [match-s cur-s rest-s] (-> (or (util/parse-patterns f) (util/sanitize f))
                                   patternize
                                   re-pattern
                                   (re-matches s))
        res (-> res
                (assoc :s rest-s)
                (cond-> (keyword? f) (assoc-in [:acc f] (util/parse-int cur-s))))]
    (if-not cur-s
      (reduced nil)
      (if rest-s
        res
        (reduced (if strict? nil res))))))

(defn priv-parse [s fmt {strict? :strict?}]
  (let [{:keys [s acc]} (reduce pv {:s s :strict? strict?} fmt)]
    (if (and s strict?) nil acc)))

(defn parse
  ([s] (priv-parse s util/iso-fmt {:strict? nil}))
  ([s fmt] (priv-parse s fmt {:strict? nil})))

(defn strict-parse
  ([s] (priv-parse s util/iso-fmt {:strict? true}))
  ([s fmt] (priv-parse s fmt {:strict? true})))

(defn format
  ([t] (format t util/iso-fmt))
  ([t fmt-vec]
   (->> fmt-vec
        (mapv (fn [x]
                (let [kw (cond-> x (vector? x) first)
                      v  (get t kw)
                      width (if (vector? x) (second x) (util/format-patterns x))]
                  (if (contains? util/format-patterns kw)
                    (format-str
                      (str "%0" width \d)
                      (or v 0)
                      width)
                    (or v x)))))
        str/join)))

(defn date-convertable? [value in out]
  (ops/eq?
   (parse value in)
   (parse (format (parse value in) out) out)))

(defn date-valid? [value fmt]
  #?(:clj true ; TODO
     :cljs (not (js/isNaN (.parse js/Date (format (parse value fmt) [:year "-" :month "-" :day]))))))
