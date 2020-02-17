(ns chrono.mask
  (:require [chrono.util :as util]))

(defn parse
  [s fmt]
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
