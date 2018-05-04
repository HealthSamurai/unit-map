(ns chrono.core)


(defn simplify [key default max [t r]]
  (let [v (get t key)]
    (vector
     (assoc t key (if v
                    (rem (+ r v) max)
                    (+ r default)))
     (if v
       (quot (+ r v) max)
       0))))

(defn normalize [t]
  (->> [t 0]
       (simplify :second 0 60)
       (simplify :minute 0 60)
       (simplify :hour 0 24)
       first))

(defn parse [s format])

(defn format [t format])

(defn timestamp [t])

(defn diff [t t'])

(defn plus [t i]
  (normalize
   (merge-with + t i)))

(defn to-tz [t tz])

(defn datetime [t])
