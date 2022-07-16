(ns unit-map.impl.registry)


(defn push-to-useq-graph [useqs-map {:as useq-info, :keys [unit eq-unit]}]
  (let [eq-useqs (when (some? eq-unit)
                   (->> (vals useqs-map)
                        (keep #(get % eq-unit))))

        to-this-unit-new (->> eq-useqs
                              (map #(assoc % :next-unit unit))
                              distinct)

        to-this-unit (->> (vals useqs-map)
                          (keep #(get % unit)))

        to-eq-new (when (some? eq-unit)
                    (->> to-this-unit
                         (map #(assoc % :next-unit eq-unit))
                         distinct))

        to-save (concat [useq-info]
                        to-this-unit-new
                        to-eq-new)]
    (reduce (fn [acc s]
              (assoc-in acc
                        [(:unit s) (:next-unit s)]
                        (dissoc s :eq-unit)))
            useqs-map
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


(defn reg-useq
  ([registry unit useq & {:keys [next-unit eq-unit]}] #_"TODO: remove this arity, use only kwargs?"
   (reg-useq registry {:unit unit, :useq useq, :next-unit next-unit, :eq-unit eq-unit}))

  ([registry {:keys [unit useq next-unit eq-unit]}]
   (let [useq-info (cond-> {:unit unit, :useq useq}
                     (some? next-unit) (assoc :next-unit next-unit)
                     (some? eq-unit)   (assoc :eq-unit eq-unit))]
     (-> registry
         (update :useqs push-to-useq-graph useq-info)
         (update :eq-units push-to-eq-units useq-info)))))


(defn usys-continuous? [registry units]
  (let [reverse-units (reverse units)]
    (->> (map vector
              (cons nil reverse-units)
              reverse-units)
         (every?
           (fn [[cur-unit prev-unit]]
             (get-in registry [:useqs prev-unit cur-unit]))))))


(defn reg-usys [registry units]
  (update registry :usyss (fnil conj #{}) units))
