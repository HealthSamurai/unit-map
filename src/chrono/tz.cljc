(ns chrono.tz
  (:require [chrono.calendar :as cal]))

;; Rule  US   1967 2006  -   Oct lastSun  2:00  0    S
;; Rule  US   1967 1973  -   Apr lastSun  2:00  1:00 D
;; Rule  US   1974 only  -   Jan 6        2:00  1:00 D
;; Rule  US   1975 only  -   Feb 23       2:00  1:00 D
;; Rule  US   1976 1986  -   Apr lastSun  2:00  1:00 D
;; Rule  US   1987 2006  -   Apr Sun>=1   2:00  1:00 D
;; Rule  US   2007 max   -   Mar Sun>=8   2:00  1:00 D
;; Rule  US   2007 max   -   Nov Sun>=1   2:00  0    S

;; Zone America/New_York
;; -5:00	US	E%sT	1920
;; -5:00	NYC	E%sT	1942
;; -5:00	US	E%sT	1946
;; -5:00	NYC	E%sT	1967
;; -5:00	US	E%sT

;; rules from tzdb like sun >= 8
(defn *more-or-eq [y m dw d]
  (let [dw' (cal/day-of-week y m d)]
    (cond (= dw' dw) d
          ;; if wed vs sun
          (> dw' dw) (+ d (- 7 dw') dw)
          (< dw' dw) (+ d (- dw dw')))))

(def more-or-eq (memoize *more-or-eq))

(defn from-utc [t tz])

(defn gen-norm [k k-next del m]
  (fn [x]
    (if-let [z (get x k)] 
      (let [ds (int (/ z del))
            s  (or (get x k-next) m)
            r  (rem z del)]
        (if (>= z m)
          (assoc x k r k-next (+ s ds))
          (assoc x k (+ del r) k-next (+ s ds -1))))
      x)))

(def normalize-ms (gen-norm :ms :sec 1000 0))
(def normalize-s  (gen-norm :sec :min 60 0))
(def normalize-mi (gen-norm :min :hour 60 0))
(def normalize-h  (gen-norm :hour :day 24 0))
(def normalize-m  (gen-norm :month :year 12 1))

(defn days-and-months [y m d]
  (if (<= 1 d 27)
    [y m d]
    (cond
      (> d 0)
      (let [num-days (cal/number-of-days y m)
            dd (- d num-days)]
        (if (<= d num-days)
          [y m d]
          (if (= m 12)
            (days-and-months (inc y) 1 dd)
            (days-and-months y (inc m) dd))))

      (<= d 0)
      (let [[num-days ny nm] (if (= m 1)
                               [(cal/number-of-days (dec y) 12) (dec y) 12]
                               [(cal/number-of-days y (dec m)) y (dec m)])
            dd (+ num-days d)]
        (if (< 0 dd)
          [ny nm dd]
          (days-and-months ny nm dd))))))

(defn normalize-d  [x]
  (if (and (:year x) (:month x) (:day x))
    (let [[y m d] (days-and-months (:year x) (:month x) (:day x))]
      (assoc x :year y :month m :day d))
    x))

(defn init-plus [r i]
  (->>
   (concat (keys (dissoc r :tz)) (keys i))
   (into #{})
   (reduce (fn [acc k] (assoc acc k (+ (get r k 0) (get i k 0)))) {})))

(defn- plus
  "time & interval"
  [t i]
  (-> (init-plus t i)
      (normalize-ms)
      (normalize-s)
      (normalize-mi)
      (normalize-h)
      (normalize-m)
      (normalize-d)))

(defmulti day-saving "[tz y]" (fn [tz _] tz))

(defmethod day-saving
  :ny
  [_ y]
  (assert (> y 2006) "Not impl.")
  {:offset 5
   :ds -1
   :in {:year y :month 3 :day (more-or-eq y 3 0 8) :hour 2 :min 0}
   :out {:year y :month 11 :day (more-or-eq y 11 0 1) :hour 2 :min 0}})

(defn *day-saving-with-utc [tz y]
  (let [ds (day-saving tz y)]
    (assoc ds
           :in-utc (plus (:in ds) {:hour (:offset ds)})
           :out-utc (plus (:out ds) {:hour (+ (:offset ds) (:ds ds))}))))

(def day-saving-with-utc (memoize *day-saving-with-utc))

(def default-time {:year 0 :month 1 :day 1 :hour 0 :min 0 :sec 0})
(def defaults-units  [[:year 0] [:month 1] [:day 1] [:hour 0] [:min 0] [:sec 0]])


(defn after? [t t']
  (loop [[[p s] & ps] defaults-units]
    (let [tp (get t p s) tp' (get t' p s)]
      (cond
        (> tp tp') true
        (= tp tp') (and (not (empty? ps)) (recur ps))
        :else false))))

(defn eq? [t t']
  (let [t (merge default-time t)
        t' (merge default-time t')]
    (and
     (= (:year t) (:year t'))
     (= (:month t) (:month t'))
     (= (:day t) (:day t'))
     (= (:hour t) (:hour t'))
     (= (:min t) (:min t'))
     (= (:sec t) (:sec t')))))

(def before=? (complement after?))

(defn to-utc [t]
  (let [ds (day-saving-with-utc (:tz t) (:year t))
        off (if (or (before=? t (:in ds)) (after? t (:out ds)))
              (:offset ds)
              (+ (:offset ds) (:ds ds)))]
    (plus (dissoc t :tz) {:hour off})))

(defn to-tz [t tz]
  (let [ds (day-saving-with-utc tz (:year t))
        off (if (or (before=? t (:in-utc ds)) (after? t (:out-utc ds)))
              (:offset ds)
              (+ (:offset ds) (:ds ds)))]
    (assoc (plus t {:hour (- off)}) :tz tz)))

;; https://alcor.concordia.ca/~gpkatch/gdate-algorithm.html
;; https://alcor.concordia.ca/~gpkatch/gdate-method.html

(defn *ddd [y]
  (+ (* 365 y)
     (quot y 4)
     (quot y -100)
     (quot y 400)))

(defn *mmm [m]
  (->
   (* 306 m)
   (+ 5)
   (quot 10)))

(defn g [y m d]
  (let [m (rem (+ m 9) 12)
        y (- y (quot m 10))]
    (+ (*ddd y) (*mmm m) (dec d))))


(defn d [g]
  (let [y (-> (* 10000 g)
              (+ 14780)
              (quot 3652425))
        ddd (- g (*ddd y))
        y   (if (< ddd 0) (dec y) y)
        ddd (if (< ddd 0) (- g (*ddd y)) ddd)
        mi (-> (* 100 ddd)
               (+ 52)
               (quot 3060))
        mm (-> (+ mi 2)
               (rem 12)
               (+ 1))
        y (+ y (-> (+ mi 2) (quot 12)))
        dd  (+ ddd (- (*mmm mi)) 1)]
    [y mm dd]))

(comment
  (g 2018 3 1)
  (d (g 2018 1 1))
  (d (g 2018 3 5))
  (d (g 2018 7 2))
  (d (- (g 2018 1 1) 1))
  (d (+ (g 2017 12 31) 1))
  (d (+ (g 2017 12 31) 370)))

(def + plus)
