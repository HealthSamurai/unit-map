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
