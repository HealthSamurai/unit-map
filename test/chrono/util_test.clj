(ns chrono.util-test
  (:require [clojure.test :refer :all]
            [chrono.util :as sut]))

(deftest leap-year-test

  (is (=  31 (sut/days-in-month {:year 2018, :month 1})))
  (is (=  28 (sut/days-in-month {:year 2018, :month 2})))

  (doseq [y [1960 1964 1968	1972 1976
             1980 1984 1988	1992 1996
             2000 2004 2008	2012 2016]]
    (is (sut/leap-year? y)))

  (is (not (sut/leap-year? 2017)))
  (is (not (sut/leap-year? 2100)))
  (is (sut/leap-year? 2000))
  (is (sut/leap-year? 2016)))
