(ns unit-map.util
  (:refer-clojure :exclude [infinite?])
  (:require [clojure.string :as str]
            [clojure.math :as math]))


(defn infinite? [x]
  (and (double? x)
       (clojure.core/infinite? x)))


(def finite? (complement infinite?))


(defn try-parse-long [s]
  (some->> s str (re-matches #"[-+]?\d+") parse-long))


(defn sanitize [s]
  (str/replace s #"[-.\+*?\[^\]$(){}=!<>|:\\]" #(str \\ %)))


(defn pad-str [p n s]
  (->> (concat (reverse s) (repeat p))
       (take n)
       reverse
       str/join))


(def pad-zero (partial pad-str \0))


(defn try-call [maybe-fn & args]
  (if (fn? maybe-fn)
    (apply maybe-fn args)
    maybe-fn))


(defn get-next-element [s x]
  (second (drop-while (partial not= x) s)))


(defn get-prev-element [s x]
  (last (take-while (partial not= x) s)))


(defn n-times [n f x]
  (loop [i n, r x]
    (if (pos? i)
      (recur (dec i) (f r))
      r)))


(defn apply-binary-pred [pred x y & more]
  (cond
    (not (pred x y)) false
    (next more)      (recur pred y (first more) (next more))
    :else            (pred y (first more))))


(defn map-v [f m]
  (reduce-kv (fn [acc k _] (update acc k f)) m m))


(defn floor [x] (long (math/floor x)))


(def ffilter (comp first filter))


(def regex-type (type #""))


(defn regex? [x]
  (instance? regex-type x))
