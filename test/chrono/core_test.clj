(ns chrono.core-test
  (:require [clojure.test :refer :all]
            [chrono.core :refer :all]))

(deftest plus-test
  (is (= (plus {:second 321}
               {:minute 58})
         {:hour 1
          :minute 3
          :second 21})))
