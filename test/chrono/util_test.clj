(ns chrono.util-test
  (:require [clojure.test :refer :all]
            [chrono.util :as sut]
            [chrono.datetime :as cd]
            [matcho.core :as matcho])
  (:import [java.util Date]))

(deftest leap-year-test

  (.getDay (Date. (- 2018 1900) 0 1))

  (sut/day-of-week 2018 1 1)

  (.getDay (Date. (- 2000 1900) 0 2))

  (sut/day-of-week 2000 1 2)

  (doseq [m (range 1 13)
          y (range 100)]
    (doseq [d (range 1 (inc (sut/days-in-month #::cd{:year (+ 2000 y), :month m})))]
      (let [y (+ 2000 y)
            ref (.getDay (Date. (- y 1900) (dec m) d))
            sam (sut/day-of-week y m d)]
        (when-not (= sam ref)
          (throw (Exception. (pr-str y "-" m "-" d " " "sam" sam " ref " ref)))))))

  (is (=  31 (sut/days-in-month #::cd{:year 2018, :month 1})))
  (is (=  28 (sut/days-in-month #::cd{:year 2018, :month 2})))

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
                      (is (= (sut/parse-name inp ::cd/month nil) res))))]
      (doall
       (map test-fn cases)))))

(deftest pad-zero-test
  (is (= "00321" (sut/pad-zero 5 "321")))
  (is (= "1"     (sut/pad-zero 1 "321")))
  (is (= "321"   (sut/pad-zero 3 "321"))))

(deftest more-or-eq-test
  ;; d(2010,3,14,7,0,0),
  (is (= 14 (sut/more-or-eq 2010 3 0 8)))
  ;; d(2010,11,7,6,0,0),
  (is (= 7 (sut/more-or-eq 2010 11 0 1)))

  ;; d(2017,3,12,7,0,0),
  (is (= 12 (sut/more-or-eq 2017 3 0 8)))

  ;; d(2017,11,5,6,0,0),
  (is (= 5 (sut/more-or-eq 2017 11 0 1)))

  ;; d(2018,3,11,7,0,0),
  (is (= 11 (sut/more-or-eq 2018 3 0 8)))

  ;; d(2018,11,4,6,0,0),
  (is (=  4 (sut/more-or-eq 2018 11 0 1)))
  ;; d(2019,3,10,7,0,0),
  (is (= 10 (sut/more-or-eq 2019 3 0 8)))
  ;; d(2019,11,3,6,0,0),
  (is (= 3 (sut/more-or-eq 2019 11 0 1)))
  ;; d(2020,3,8,7,0,0),
  (is (= 8 (sut/more-or-eq 2020 3 0 8)))
  ;; d(2020,11,1,6,0,0),
  (is (= 1 (sut/more-or-eq 2020 11 0 1))))
