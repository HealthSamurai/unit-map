(ns chrono.calendar-test
  (:require [chrono.calendar :as cal]
            [clojure.test :refer :all]
            [matcho.core :as matcho])
  (:import [java.util Date]))

(deftest calendar-test

  (is (=  31 (cal/number-of-days 2018 1)))
  (is (=  28 (cal/number-of-days 2018 2)))

  (doseq [y [1960 1964 1968	1972 1976
             1980 1984 1988	1992 1996
             2000 2004 2008	2012 2016]]
    (is (cal/is-leap? y)))

  (.getDay (Date. (- 2018 1900) 0 1))

  (cal/day-of-week 2018 1 1)

  (.getDay (Date. (- 2000 1900) 0 2))

  (cal/day-of-week 2000 1 2)


  (doseq [m (range 1 13)
          y (range 100)]
    (doseq [d (range 1 (inc (cal/number-of-days (+ 2000 y) m)))]
      (let [y (+ 2000 y)
            ref (.getDay (Date. (- y 1900) (dec m) d))
            sam (cal/day-of-week y m d)]
        (when-not (= sam ref)
          (throw (Exception. (pr-str y "-" m "-" d " " "sam" sam " ref " ref)))))))

  (matcho/match
   (cal/for-month 2018 3)
   {:y 2018 :m 3
    :cal [[{:m 2 :d 25} {:m 2 :d 26} {:m 2 :d 27} {:m 2 :d 28} {:m 3 :d 1} {:m 3 :d 2} {:m 3 :d 3}]
          [{:m 3 :d 4} {:m 3 :d 5} {:m 3 :d 6} {:m 3 :d 7} {:m 3 :d 8} {:m 3 :d 9} {:m 3 :d 10}]
          [{:m 3 :d 11} {:m 3 :d 12} {:m 3 :d 13} {:m 3 :d 14} {:m 3 :d 15} {:m 3 :d 16} {:m 3 :d 17}]
          []
          []
          [{:m 4 :d 1} {:m 4 :d 2} {:m 4 :d 3} {:m 4 :d 4} {:m 4 :d 5} {:m 4 :d 6} {:m 4 :d 7}]]})


  )
