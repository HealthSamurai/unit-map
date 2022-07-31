(ns unit-map.impl.io
  (:refer-clojure :exclude [format])
  (:require [unit-map.impl.util :as util]
            [clojure.string :as str]))


;; TODO: move all date time related consts to type definition
;; TODO: add convert from/to java/js date objects


;;;;;;;;;; registrator & registry


(defn reg-format! [registry-atom format-name format-params]
  (swap! registry-atom assoc-in [:format format-name] format-params))


;;;;;;;;;; reader


(defn read-format-params [fmt-el]
  (cond
    (map? fmt-el)
    fmt-el

    (vector? fmt-el)
    (let [[value & rest-fmt] fmt-el
          width (util/ffilter integer? rest-fmt)]
      {:value value
       :width width
       :pad   (when width
                (or (util/ffilter (some-fn string? char?) rest-fmt)
                    \space))})

    :else
    {:value fmt-el}))


(defn determine-value [registry value]
  (cond
    (not (keyword? value))
    {:separator value}

    (contains? (:format registry) value)
    {:element value}

    :else
    {:unit value}))


#_"TODO: maybe make this as a data-reader for fmt-vec?"
(defn read-fmt-el [registry fmt-el]
  (let [format-params (read-format-params fmt-el)]
    (cond-> format-params
      (contains? format-params :value)
      (merge (determine-value registry (:value format-params))))))


;;;;;;;;;; parse


(defn parse-val [fmt-el x]
  (util/try-parse-long (str/trim x)))


(defn el->regex [{:keys [value width]}]
  (cond
    (util/regex? value) value
    (some? width)       (apply str (repeat width \.))
    (string? value)     (util/sanitize value)
    (char? value)       (util/sanitize value)
    :else               ".+?"))


(defn mk-group-regex [cur-group next-group]
  (let [{el-regex :regex, :as el} (last cur-group)

        cur-group-border  (map :regex (butlast cur-group))
        next-group-border (:regex (first next-group))
        group-regex       (str "(?:" (str/join cur-group-border)
                               "(" el-regex ")"
                               "(?=(?:" next-group-border "|$)))")]

    (assoc el :group-regex group-regex)))


(defn make-regex-groups [registry fmt-vec]
  (let [acc (reduce (fn [acc fmt-el]
                      (let [el (as-> fmt-el $
                                 (read-fmt-el registry $)
                                 (assoc $ :regex (el->regex $)))]
                        (if (keyword? (:value el))
                          {:group []
                           :result (-> (:result acc)
                                       (conj (conj (:group acc) el)))}
                          (update acc :group conj el))))
                    {:group [], :result []}
                    (concat [#"^"] fmt-vec [#"$"]))

        result (conj (:result acc) (:group acc))]

    (mapv mk-group-regex result (rest result))))


(declare parse-unit)


(defn parse-res [registry parse-params parsed]
  (cond
    (:unit parse-params)
    {(:unit parse-params) parsed}

    (:units parse-params)
    (into {}
          (keep (fn [u] (when-let [v (get parsed u)]
                          [u v])))
          (:units parse-params))

    (:element parse-params)
    (parse-unit registry
                (get-in registry [:format (:element parse-params)])
                parsed)

    (:elements parse-params)
    (into {}
          (map #(parse-unit registry
                            (get-in registry [:format %])
                            (get parsed %)))
          (:elements parse-params))))


(defn parse-unit [registry parse-params value-s]
  (when (not (str/blank? value-s))
    (let [s (str/trim value-s)
          parsed (if-let [parse-fn (:parse parse-params)]
                   (try (parse-fn registry s)
                        (catch Exception _)) #_"NOTE: should it suppress parsing errors?"
                   s)]
      (some->> parsed
               (parse-res registry parse-params)
               not-empty))))


(defn parse-groups [registry acc s [el & rest-els] & {:keys [strict]}]
  (cond
    (not (str/blank? s))
    (let [pat (re-pattern (str (:group-regex el) "(.*$)?"))

          [match-s cur-s rest-s] (re-find pat s)
          found?                 (not (str/blank? match-s))]
      (when (or (not strict) found?)
        (recur registry
               (cond-> acc
                 found?
                 (merge (parse-unit registry el cur-s)))
               rest-s
               rest-els
               {:strict strict})))

    (or (not strict) (empty? el))
    acc))


(defn parse [registry s fmt-vec & {:keys [strict]}]
  (parse-groups registry {} s (make-regex-groups registry fmt-vec) :strict strict))


;;;;;;;;;; format


(declare format-unit)


(defn format-args [registry format-params umap]
  (cond
    (:unit format-params)
    (get umap (:unit format-params))

    (:element format-params)
    (format-unit registry
                 (get-in registry
                         [:format (:element format-params)])
                 umap)

    (:units format-params)
    (->> (:units format-params)
         (into {}
               (keep (fn [unit]
                       (when-let [v (get umap unit)]
                         [unit v]))))
         not-empty)

    (:elements format-params)
    (->> (:elements format-params)
         (into {} (keep (fn [fmt-key]
                          (when-let [v (format-unit
                                         registry
                                         (get-in registry [:format fmt-key])
                                         umap)]
                            [fmt-key v]))))
         not-empty)))


(defn format-unit [registry format-params umap]
  (when (seq umap)
    (let [arg       (format-args registry format-params umap)
          format-fn (:format format-params)
          width     (:width format-params)
          pad       (:pad format-params " ")]
      (when (some? arg)
        (cond->> arg
          format-fn (format-fn registry)
          :always   str
          width     (util/pad-str pad width))))))


(defn format-el [registry fmt-el umap]
  (let [format-params (read-fmt-el registry fmt-el)]
    (or (:separator format-params)
        (format-unit registry
                     format-params
                     umap))))


(defn format [registry umap fmt-vec]
  (->> fmt-vec
       (map #(format-el registry % umap))
       str/join))


;;;;;;;;;; util


#_(defn convertable? [value in out]
    (let [v (parse value in)]
      (ops/eq? v (-> v  (format out) (parse out)))))


#_(defn valid? [s fmt]
    (let [d (parse s fmt)]
      (ops/eq? d (ops/ensure-less-significant-units d))))
