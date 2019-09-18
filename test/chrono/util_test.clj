(ns chrono.util-test
  (:require [clojure.test :refer :all]
            [chrono.util :as sut]))

(deftest leap-year-test
  (is (not (sut/leap-year? 2017)))
  (is (not (sut/leap-year? 2100)))
  (is (sut/leap-year? 2000))
  (is (sut/leap-year? 2016)))
