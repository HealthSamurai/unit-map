(ns chrono.tz-test
  (:require [chrono.tz]
            [chrono.ops :as sut]
            [clojure.test :refer :all]
            [matcho.core :as matcho]))

(deftest test-ny-daysaving-with-utc
  (matcho/match
   (sut/day-saving-with-utc :ny 2017)
   {:in     {:month 3 :day 12 :hour 2 :min 0}
    :in-utc {:month 3 :day 12 :hour 7}

    :out     {:month 11 :day 5 :hour 2}
    :out-utc {:month 11 :day 5 :hour 6}})

  (matcho/match
   (sut/day-saving-with-utc :ny 2018)
   {:in  {:month 3 :day 11}
    :out {:month 11 :day 4}})

  (matcho/match
   (sut/day-saving-with-utc :ny 2019)
   {:in  {:month 3 :day 10}
    :out {:month 11 :day 3}}))
