(ns chrono.core)

(def fields-hierarchy
  [[:second 0 60 :minute]
   [:minute 0 60]
   [:hour 0 24]
   :day
   :month
   :year])

(defmulti normalize-field (fn [k _] (if (vector? k) (first k) k)))

(defmethod normalize-field :default [k t]
  )


(defn normalize [t]
  (reduce (fn [t key]
            (merge t (normalize-field key t)))
          t
          fields-hierarchy))

(defn parse [s format])

(defn format [t format])

(defn timestamp [t])

(defn diff [t t'])

(defn plus [t i]
  (normalize
   (merge-with + t i)))

(defn to-tz [t tz])

(defn datetime [t])
