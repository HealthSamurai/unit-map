(ns chrono.util)

(defn leap-year? [y]
  (and (= 0 (rem y 4))
       (or (not= 0 (rem y 100))
           (= 0 (rem y 400)))))

(defn- days-in-month [f]
  (case (:month f)
    2 (if (leap-year? (:year f)) 29 28)
    (1 3 5 7 8 10 12) 31
    30))

(defn- simplify
  ([key acc] (simplify key nil acc))
  ([key max [t r]]
   (let [v (get t key)]
     (vector
      (assoc t key (if v
                     (if max
                       (rem (+ r v) max)
                       (+ r v))
                     r))
      (if (and v max)
        (quot (+ r v) max)
        0)))))

(defn- add-days [t r]
  (update t :day #(+ r (or % 1))))

(defn- simplify-month [f]
  (-> f
      (update :month #(rem % 12))
      (update :year #(+ % (quot (:month f) 12)))))

(defn- simplify-day [f]
  (-> f
      (update :day #(- % (days-in-month f)))
      (update :month inc)))

(defn- simplify-date [f]
  (cond (< 12 (:month f))
        (simplify-date (simplify-month f))
        (< (days-in-month f) (:day f))
        (simplify-date (simplify-day f))
        :else f))

(defn normalize [t]
  (case (:type t)
    :datetime (->> [t 0]
                   (simplify :second 60)
                   (simplify :minute 60)
                   (simplify :hour 24)
                   (apply add-days)
                   (simplify-date)
                   )
    :time (->> [t 0]
               (simplify :second 60)
               (simplify :minute 60)
               (simplify :hour)
               first)
    t))
