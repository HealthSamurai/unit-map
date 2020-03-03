(ns chrono.io
  (:require [chrono.util :as util]
            [chrono.ops :as ops]
            [clojure.string :as str]
            #?(:cljs [goog.string])
            #?(:cljs [goog.string.format]))
  (:refer-clojure :exclude [format]))

(defn- format-str [fmt & args]
  (apply
   #?(:clj  clojure.core/format
      :cljs goog.string/format)
   fmt
   args))

(defn parse
  ([s] (parse s util/iso-fmt))
  ([s fmt]
   (let [fmt (map #(cond-> % (vector? %) first) fmt)
         pat (map #(or (util/parse-patterns %) (util/sanitize %)) fmt)]
     (loop [s            s
            [f & rest-f] fmt
            acc          nil]
       (if-not (and s f)
         acc
         (let [p   (or (util/parse-patterns f) (util/sanitize f))
               pat (re-pattern (str "(" p ")" "(.+)?"))

               [match-s cur-s rest-s] (re-matches pat s)]
           (when match-s
             (recur rest-s rest-f
                    (cond-> acc
                      (contains? util/parse-patterns f)
                      (assoc f (util/parse-int cur-s)))))))))))

(defn strict-parse
  ([s] (strict-parse s util/iso-fmt))
  ([s fmt]
   (let [fmt (map #(cond-> % (vector? %) first) fmt)
         pat (map #(or (util/parse-patterns %) (util/sanitize %)) fmt)]
     (loop [s            s
            [f & rest-f] fmt
            [p & rest-p] pat
            acc          nil]
       (if-not (and s f)
         acc
         (let [ahead (apply str rest-p)
               pat   (->> (when (seq rest-p) (str \( ahead \) ))
                          (str "(" p ")")
                          re-pattern)

               [match-s cur-s rest-s] (re-matches pat s)]
           (when match-s
             (recur rest-s rest-f rest-p
                    (cond-> acc
                      (contains? util/parse-patterns f)
                      (assoc f (util/parse-int cur-s)))))))))))

(defn format
  ([t] (format t util/iso-fmt))
  ([t fmt-vec]
   (->> fmt-vec
        (mapv (fn [x]
                (let [kw (cond-> x (vector? x) first)
                      v  (get t kw)]
                  (if (contains? util/format-patterns kw)
                    (format-str (str "%0" (if (vector? x) (second x) (util/format-patterns x)) \d) (or v 0))
                    (or v x)))))
        str/join)))

(defn date-convertable? [value in out]
  (ops/eq?
   (parse value in)
   (parse (format (parse value in) out) out)))

(defn date-valid? [value fmt]
  #?(:clj true ; TODO
     :cljs (not (js/isNaN (.parse js/Date (format (parse value fmt) [:year "-" :month "-" :day]))))))
