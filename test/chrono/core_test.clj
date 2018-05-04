(ns chrono.core-test
  (:require [clojure.test :refer :all]
            [chrono.core :refer :all]))

(deftest plus-test
  (testing "time intervals"
    (is (= (plus {:type :time
                  :second 321}
                 {:minute 58})
           {:type :time
            :hour 1
            :minute 3
            :second 21})))

  (testing "datetime"
    (is (= (plus (datetime {:year 1999
                            :month 6
                            :day 28})
                 {:hour 24
                  :month 8
                  :minute 10})
           {:type :datetime
            :year 2000
            :month 2
            :day 29
            :hour 0
            :minute 10
            :second 0}))))
