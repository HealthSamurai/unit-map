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

(defn- format-str [fmt val width]
  (->>
    (apply
     #?(:clj  clojure.core/format
         :cljs goog.string/format)
      fmt
      [val])
    (take-last width)
    (str/join)))

(defn parse
  ([s] (parse s util/iso-fmt))
  ([s fmt]
   (let [lang (-> fmt meta first)
         fmt (map #(cond-> % (vector? %) first) fmt)
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
                      (assoc f (util/parse-val cur-s
                                               {:unit f :lang lang})))))))))))

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
