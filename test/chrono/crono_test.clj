(ns chrono.crono-test
  (:require [chrono.crono :as sut]
            [chrono.datetime :as cd]
            [chrono.interval :as ci]
            [clojure.test :refer :all]))

(deftest crono-test
  (is (= #::cd{:year 2020 :month 1 :day 1 :hour 12}
         (sut/next-time #::cd{:year 2020 :month 1 :day 1 :hour 11}
                        {:every ::ci/day :at {::cd/hour 12}})))

  (is (= #::cd{:year 2020 :month 1 :day 2 :hour 12}
         (sut/next-time #::cd{:year 2020 :month 1 :day 1 :hour 12 :min 10}
                        {:every ::ci/day :at {::cd/hour 12}})))

  (is (= #::cd{:year 2020 :month 1 :day 1 :hour 14}
         (sut/next-time #::cd{:year 2020 :month 1 :day 1 :hour 13}
                        {:every ::ci/day :at [{::cd/hour 12}
                                              {::cd/hour 14}]})))

  (is (= #::cd{:year 2020 :month 1 :day 1 :hour 10 :min 30}
         (sut/next-time #::cd{:year 2020 :month 1 :day 1 :hour 10 :min 13}
                        {:every ::ci/hour :at [{::cd/min 0} {::cd/min 30}]})))

  (is (= #::cd{:year 2020 :month 1 :day 1 :hour 12}
         (sut/next-time #::cd{:year 2020 :month 1 :day 1 :hour 11 :min 43}
                        {:every ::ci/hour :at [{::cd/min 0} {::cd/min 30}]})))

  (is (= #::cd{:year 2020 :month 1 :day 1 :hour 12 :min 10}
         (sut/next-time #::cd{:year 2020 :month 1 :day 1 :hour 12 :min 7}
                        {:every ::ci/hour :at [{::cd/min 0}
                                               {::cd/min 5}
                                               {::cd/min 10}
                                               {::cd/min 15}
                                               {::cd/min 20}
                                               {::cd/min 25}
                                               {::cd/min 30}
                                               {::cd/min 35}
                                               {::cd/min 40}
                                               {::cd/min 45}
                                               {::cd/min 50}
                                               {::cd/min 55}]})))

  (is (= #::cd{:year 2020 :month 1 :day 1 :hour 11}
         (sut/next-time #::cd{:year 2020 :month 1 :day 1 :hour 10 :min 55}
                        {:every ::ci/hour :at [{::cd/min 0}
                                               {::cd/min 5}
                                               {::cd/min 10}
                                               {::cd/min 15}
                                               {::cd/min 20}
                                               {::cd/min 25}
                                               {::cd/min 30}
                                               {::cd/min 35}
                                               {::cd/min 40}
                                               {::cd/min 45}
                                               {::cd/min 50}
                                               {::cd/min 55}]})))

  (is (= #::cd{:year 2020 :month 1 :day 1 :hour 12}
         (sut/next-time #::cd{:year 2020 :month 1 :day 1 :hour 11 :min 9 :sec 10}
                        {:every ::ci/hour})))

  (is (= #::cd{:year 2020 :month 1 :day 1 :hour 11 :min 10}
         (sut/next-time #::cd{:year 2020 :month 1 :day 1 :hour 11 :min 9 :sec 10}
                        {:every ::ci/min})))

  (is (= #::cd{:year 2020 :month 1 :day 1 :hour 11 :min 10}
         (sut/next-time #::cd{:year 2020 :month 1 :day 1 :hour 11 :min 9 :sec 10}
                        {:every ::ci/min :at {::cd/sec 0}})))

  (is (= #::cd{:year 2020 :month 1 :day 1 :hour 11 :min 10 :sec 10}
         (sut/next-time #::cd{:year 2020 :month 1 :day 1 :hour 11 :min 9 :sec 10}
                        {:every ::ci/min :at {::cd/sec 10}})))


  (is (= true
         (sut/now? #::cd{:year 2020 :month 1 :day 1 :hour 12 :min 1}
                   {:every ::ci/day
                    :at {::cd/hour 12}
                    :until {::cd/hour 12 ::cd/min 30}})))

  (is (= false
         (sut/now? #::cd{:year 2020 :month 1 :day 1 :hour 12 :min 31}
                   {:every ::ci/day
                    :at {::cd/hour 12}
                    :until {::cd/hour 12 ::cd/min 30}})))

  (is (= true
         (sut/now? #::cd{:year 2020 :month 1 :day 1 :hour 12 :min 31}
                   {:every ::ci/day
                    :at {::cd/hour 12}})))

  (is (= #::cd{:year 2020 :month 5 :day 18 :hour 10}
         (sut/next-time #::cd{:year 2020 :month 5 :day 17 :hour 9}
                        {:every "monday" :at {::cd/hour 10}})))

  (is (= #::cd{:year 2020 :month 5 :day 18 :hour 10}
         (sut/next-time #::cd{:year 2020 :month 5 :day 18 :hour 9}
                        {:every "monday" :at {::cd/hour 10}})))

  (is (= #::cd{:year 2020 :month 5 :day 26 :hour 10}
         (sut/next-time #::cd{:year 2020 :month 5 :day 19 :hour 11}
                        {:every "tuesday" :at {::cd/hour 10}})))

  (is (= #::cd{:year 2020 :month 5 :day 19 :hour 10}
         (sut/next-time #::cd{:year 2020 :month 5 :day 18 :hour 11}
                        {:every "tuesday" :at {::cd/hour 10}}))))
