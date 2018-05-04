(ns chrono.core-test
  (:require [clojure.test :refer :all]
            [chrono.core :refer :all]))

(deftest plus-test
  (is (= (plus {:year 2018
                :day 4}
               {:month 13
                :day 2})
         {:year 2019
          :month 1
          :day 6
          :hour 0
          :minute 0
          :second 0})))
