(ns chrono.io
  (:require [chrono.util :as util]
            [chrono.new-ops :as new-ops]
            [clojure.string :as str])
  (:refer-clojure :exclude [format]))


(defn parse [s fmt & {:keys [strict], :or {strict false}}]
  (let [fmts (map #(cond-> % (vector? %) first) fmt)
        pat  (map (some-fn util/parse-patterns util/sanitize) fmts)]
    (loop [s            s
           [f & rest-f] fmts
           [p & rest-p] pat
           acc          (with-meta {} (meta fmt))]
      (if-not (and s f)
        acc
        (let [pat (->> (if strict
                         (str \( (apply str rest-p) \))
                         "(.+)?")
                       (str "(" p ")")
                       re-pattern)

              [match-s cur-s rest-s] (re-matches pat s)]
          (cond
            match-s (recur rest-s rest-f rest-p
                           (cond-> acc
                             (contains? util/parse-patterns f)
                             (assoc f (util/parse-int cur-s))))

            (not strict) acc))))))


(defn format
  ([t fmt-vec]
   (letfn [(format-el [value fmt-el]
             (let [[fmt pad-width pad-str] (flatten (vector fmt-el))

                   unit-value (or (get value fmt)
                                  (new-ops/sequence-nth (new-ops/unit-rule value fmt) value 0)
                                  fmt)
                   pad-width  (or pad-width (util/format-patterns fmt) (count (str unit-value)))
                   pad-str    (or pad-str
                                  (when (number? unit-value) 0)
                                  " ")]
               (util/pad-str pad-str pad-width (str unit-value))))]
     (str/join (map (partial format-el t) fmt-vec)))))


(defn date-convertable? [value in out]
  (let [v (parse value in)]
    (new-ops/eq? v (-> v  (format out) (parse out)))))


(defn date-valid? [s fmt]
  (let [d (parse s fmt)]
    (new-ops/eq? d (new-ops/ensure-less-significant-units d))))
