(ns chrono.tz-test
  (:require [chrono.tz]
            [chrono.ops :as sut]
            [chrono.datetime :as cd]
            [clojure.test :refer :all]
            [matcho.core :as matcho]))

(deftest test-ny-daysaving
  (matcho/match (sut/day-saving :ny 2017)
                {:in  #::cd{:month 3 :day 12 :hour 2 :min 0}
                 :out #::cd{:month 11 :day 5 :hour 2}})

  (matcho/match (sut/day-saving-with-utc :ny 2017)
                {:in     #::cd{:month 3 :day 12 :hour 2 :min 0}
                 :in-utc #::cd{:month 3 :day 12 :hour 7}

                 :out     #::cd{:month 11 :day 5 :hour 2}
                 :out-utc #::cd{:month 11 :day 5 :hour 6}})

  (matcho/match (sut/day-saving-with-utc :ny 2018)
                {:in  #::cd{:month 3 :day 11}
                 :out #::cd{:month 11 :day 4}})

  (matcho/match (sut/day-saving-with-utc :ny 2019)
                {:in  #::cd{:month 3 :day 10}
                 :out #::cd{:month 11 :day 3}}))

(deftest test-ny-daysaving-with-utc
  (matcho/match
   (sut/day-saving-with-utc :ny 2017)
   {:in     #::cd{:month 3 :day 12 :hour 2 :min 0}
    :in-utc #::cd{:month 3 :day 12 :hour 7}

    :out     #::cd{:month 11 :day 5 :hour 2}
    :out-utc #::cd{:month 11 :day 5 :hour 6}})

  (matcho/match
   (sut/day-saving-with-utc :ny 2018)
   {:in  #::cd{:month 3 :day 11}
    :out #::cd{:month 11 :day 4}})

  (matcho/match
   (sut/day-saving-with-utc :ny 2019)
   {:in  #::cd{:month 3 :day 10}
    :out #::cd{:month 11 :day 3}}))
