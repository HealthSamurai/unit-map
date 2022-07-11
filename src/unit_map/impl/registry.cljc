(ns unit-map.impl.registry)


(defn push-to-seq-graph [seqs-map unit useq]
  (let [eq-seqs (when-let [eq-unit (:eq-unit useq)]
                  (->> (vals seqs-map)
                       (keep #(get % eq-unit))))

        to-this-unit-new (->> eq-seqs
                              (map #(assoc % :next-unit unit))
                              distinct)

        to-this-unit (->> (vals seqs-map)
                          (keep #(get % unit)))

        to-eq-new (when-let [eq-unit (:eq-unit useq)]
                    (->> to-this-unit
                         (map #(assoc % :next-unit eq-unit))
                         distinct))

        this-seq (assoc useq :unit unit)

        to-save (concat [this-seq]
                        to-this-unit-new
                        to-eq-new)]
    (reduce (fn [acc s]
              (assoc-in acc
                        [(:unit s) (:next-unit s)]
                        (dissoc s :eq-unit)))
            seqs-map
            to-save)))


(defn push-to-eq-units [eq-units-sets unit {:keys [eq-unit]}]
  (let [group (->> eq-units-sets
                   (filter #(or (get % unit) (get % eq-unit)))
                   first)
        new-group (-> (or group #{})
                      (conj unit)
                      (cond-> (some? eq-unit) (conj eq-unit)))]
    (-> (or eq-units-sets #{})
        (disj group)
        (conj new-group))))


(defn reg-useq [registry unit useq]
  (-> registry
      (update :seqs push-to-seq-graph unit useq)
      (update :eq-units push-to-eq-units unit useq)))


(defn sys-continuous? [registry units]
  (let [reverse-units (reverse units)]
    (->> (map vector
              (cons nil reverse-units)
              reverse-units)
         (every?
           (fn [[cur-unit prev-unit]]
             (get-in registry [:seqs prev-unit cur-unit]))))))
