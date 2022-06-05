(ns unit-map.type.chrono.util.calendar-test
  (:require [unit-map.type.chrono.util.calendar :as sut]
            [clojure.test :refer [deftest]]
            [matcho.core :as matcho]))

(deftest calendar-test

  (t/is (=
   {:year 2018 :month 3
    :cal [[{:month 2 :day 25} {:month 2 :day 26} {:month 2 :day 27} {:month 2 :day 28} {:month 3 :day 1} {:month 3 :day 2} {:month 3 :day 3}]
          [{:month 3 :day 4} {:month 3 :day 5} {:month 3 :day 6} {:month 3 :day 7} {:month 3 :day 8} {:month 3 :day 9} {:month 3 :day 10}]
          [{:month 3 :day 11} {:month 3 :day 12} {:month 3 :day 13} {:month 3 :day 14} {:month 3 :day 15} {:month 3 :day 16} {:month 3 :day 17}]
          []
          []
          [{:month 4 :day 1} {:month 4 :day 2} {:month 4 :day 3} {:month 4 :day 4} {:month 4 :day 5} {:month 4 :day 6} {:month 4 :day 7}]]}
   (sut/for-month 2018 3))))
