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

(deftest parse-name-test
  (testing "testing months parsing \n"
    (let [cases {"jan" 1
                 "FEB" 2
                 "March" 3
                 "april" 4
                 "MaY." 5
                 "JUNE" 6}
          test-fn (fn [[inp res]]
                    (testing (str "parsing: " inp)
                      (is (= (sut/parse-name inp :month nil) res))))]
      (doall
       (map test-fn cases)))))

(deftest pad-zero-test
  (is (= "00321" (sut/pad-zero 5 "321")))
  (is (= "1"     (sut/pad-zero 1 "321")))
  (is (= "321"   (sut/pad-zero 3 "321"))))
