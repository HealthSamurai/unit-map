(ns chrono.mask
  (:require [chrono.util :as util]
            [clojure.string :as str]
            #?(:cljs [goog.string])
            #?(:cljs [goog.string.format])))

(defn- format-str [fmt & args]
  (apply
   #?(:clj  clojure.core/format
      :cljs goog.string/format)
   fmt
   args))

(defn parse [s fmt]
  (let [fmts (map #(cond-> % (vector? %) first) fmt)
        pats (map #(or (util/parse-patterns %) (util/sanitize %)) fmts)]
    (loop [s            s
           [f & rest-f :as fmts] fmts
           [p & rest-p :as pats] pats
           acc          nil]
      (if-not (and s f)
        acc
        (let [ahead "(.+)?"
              pat   (re-pattern (str "(" p ")" ahead))
              [match-s cur-s rest-s] (re-matches pat s)]
          (if match-s
            (recur rest-s rest-f rest-p
                   (cond-> acc
                     (contains? util/parse-patterns f)
                     (assoc f (util/parse-int cur-s))))
            (if-not (keyword? f)
              (recur (str f s)  fmts pats acc)
              acc)))))))


(defn build [t fmt]
  (reduce (fn [acc f]
            (let [kw (cond-> f (vector? f) first)
                  v  (get t kw)]
              (cond
                (not (contains? util/format-patterns kw))
                (str acc f)

                (some? v)
                (str acc (format-str (str "%0" (if (vector? f) (second f) (util/format-patterns f)) \d) v))

                :else (reduced acc))))
          ""
          fmt))

(defn resolve [s fmt]
  (build (parse s fmt) fmt))
