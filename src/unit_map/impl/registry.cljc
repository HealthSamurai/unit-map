(ns unit-map.impl.registry)


(defn push-to-seq-graph [seqs-map {:as seq-info, :keys [unit eq-unit]}]
  (let [eq-seqs (when (some? eq-unit)
                  (->> (vals seqs-map)
                       (keep #(get % eq-unit))))

        to-this-unit-new (->> eq-seqs
                              (map #(assoc % :next-unit unit))
                              distinct)

        to-this-unit (->> (vals seqs-map)
                          (keep #(get % unit)))

        to-eq-new (when (some? eq-unit)
                    (->> to-this-unit
                         (map #(assoc % :next-unit eq-unit))
                         distinct))

        to-save (concat [seq-info]
                        to-this-unit-new
                        to-eq-new)]
    (reduce (fn [acc s]
              (assoc-in acc
                        [(:unit s) (:next-unit s)]
                        (dissoc s :eq-unit)))
            seqs-map
            to-save)))


(defn push-to-eq-units [eq-units-sets {:keys [unit eq-unit]}]
  (let [group (->> eq-units-sets
                   (filter #(or (get % unit) (get % eq-unit)))
                   first)
        new-group (-> (or group #{})
                      (conj unit)
                      (cond-> (some? eq-unit) (conj eq-unit)))]
    (-> (or eq-units-sets #{})
        (disj group)
        (conj new-group))))


(defn reg-seq [registry unit useq & {:keys [next-unit eq-unit]}]
  (let [useq-info (cond-> {:unit unit, :useq (:useq useq)}
                    (some? next-unit) (assoc :next-unit next-unit)
                    (some? eq-unit)   (assoc :eq-unit eq-unit))]
    (-> registry
        (update :seqs push-to-seq-graph useq-info)
        (update :eq-units push-to-eq-units useq-info))))


(defn sys-continuous? [registry units]
  (let [reverse-units (reverse units)]
    (->> (map vector
              (cons nil reverse-units)
              reverse-units)
         (every?
           (fn [[cur-unit prev-unit]]
             (get-in registry [:seqs prev-unit cur-unit]))))))


(defn reg-sys [registry sys-name units]
  (assoc-in registry [:systems sys-name] units))
