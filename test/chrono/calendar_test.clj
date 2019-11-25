(ns chrono.calendar-test
  (:require [chrono.calendar :as sut]
            [chrono.util :as u]
            [clojure.test :refer [deftest]]
            [matcho.core :as matcho])
  (:import [java.util Date]))

(deftest calendar-test

  (.getDay (Date. (- 2018 1900) 0 1))

  (sut/day-of-week 2018 1 1)

  (.getDay (Date. (- 2000 1900) 0 2))

  (sut/day-of-week 2000 1 2)

  (doseq [m (range 1 13)
          y (range 100)]
    (doseq [d (range 1 (inc (u/days-in-month {:year (+ 2000 y), :month m})))]
      (let [y (+ 2000 y)
            ref (.getDay (Date. (- y 1900) (dec m) d))
            sam (sut/day-of-week y m d)]
        (when-not (= sam ref)
          (throw (Exception. (pr-str y "-" m "-" d " " "sam" sam " ref " ref)))))))

  (matcho/match
   (sut/for-month 2018 3)
   {:year 2018 :month 3
    :cal [[{:month 2 :day 25} {:month 2 :day 26} {:month 2 :day 27} {:month 2 :day 28} {:month 3 :day 1} {:month 3 :day 2} {:month 3 :day 3}]
          [{:month 3 :day 4} {:month 3 :day 5} {:month 3 :day 6} {:month 3 :day 7} {:month 3 :day 8} {:month 3 :day 9} {:month 3 :day 10}]
          [{:month 3 :day 11} {:month 3 :day 12} {:month 3 :day 13} {:month 3 :day 14} {:month 3 :day 15} {:month 3 :day 16} {:month 3 :day 17}]
          []
          []
          [{:month 4 :day 1} {:month 4 :day 2} {:month 4 :day 3} {:month 4 :day 4} {:month 4 :day 5} {:month 4 :day 6} {:month 4 :day 7}]]})


  )
