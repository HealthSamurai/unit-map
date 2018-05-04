(ns chrono.util-test
  (:require [clojure.test :refer :all]
            [chrono.util :refer :all]))

(deftest leap-year-test
  (is (not (leap-year? 2017)))
  (is (not (leap-year? 2100)))
  (is (leap-year? 2000))
  (is (leap-year? 2016)))
