(ns chrono.calendar-test
  (:require [chrono.calendar :as sut]
            [chrono.datetime :as cd]
            [clojure.test :refer [deftest]]
            [matcho.core :as matcho]))

(deftest calendar-test
  (matcho/match
   (sut/for-month 2018 3)
   {::cd/year 2018
    ::cd/month 3
    :cal [[#::cd{:month 2 :day 25}
           #::cd{:month 2 :day 26}
           #::cd{:month 2 :day 27}
           #::cd{:month 2 :day 28}
           #::cd{:month 3 :day 1}
           #::cd{:month 3 :day 2}
           #::cd{:month 3 :day 3}]
          [#::cd{:month 3 :day 4}
           #::cd{:month 3 :day 5}
           #::cd{:month 3 :day 6}
           #::cd{:month 3 :day 7}
           #::cd{:month 3 :day 8}
           #::cd{:month 3 :day 9}
           #::cd{:month 3 :day 10}]
          [#::cd{:month 3 :day 11}
           #::cd{:month 3 :day 12}
           #::cd{:month 3 :day 13}
           #::cd{:month 3 :day 14}
           #::cd{:month 3 :day 15}
           #::cd{:month 3 :day 16}
           #::cd{:month 3 :day 17}]
          []
          []
          [#::cd{:month 4 :day 1}
           #::cd{:month 4 :day 2}
           #::cd{:month 4 :day 3}
           #::cd{:month 4 :day 4}
           #::cd{:month 4 :day 5}
           #::cd{:month 4 :day 6}
           #::cd{:month 4 :day 7}]]}))
