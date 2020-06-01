(ns chrono.tz-test
  (:require [chrono.tz :as sut]
            [clojure.test :refer :all]
            [matcho.core :as matcho])
  (:import [java.util Date]))

(deftest tz-test
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
  (is (= 1 (sut/more-or-eq 2020 11 0 1)))

  ;; (is (= 5 (sut/offset-for {:year 2010 :month 1 :day 1 :hour 0  :tz :ny})))
  ;; (is (= 5 (sut/offset-for {:year 2010 :month 2 :day 1 :hour 0  :tz :ny})))
  ;; (is (= 5 (sut/offset-for {:year 2010 :month 3 :day 1 :hour 0  :tz :ny})))
  ;; (is (= 5 (sut/offset-for {:year 2010 :month 3 :day 14 :hour 0 :tz :ny})))
  ;; (is (= 4 (sut/offset-for {:year 2010 :month 3 :day 14 :hour 2 :tz :ny})))
  ;; (is (= 4 (sut/offset-for {:year 2010 :month 3 :day 20 :hour 0 :tz :ny})))
  ;; (is (= 4 (sut/offset-for {:year 2010 :month 4 :day 1 :hour 0  :tz :ny})))
  ;; (is (= 4 (sut/offset-for {:year 2010 :month 5 :day 1 :hour 0  :tz :ny})))
  ;; (is (= 4 (sut/offset-for {:year 2010 :month 6 :day 1 :hour 0  :tz :ny})))
  ;; (is (= 4 (sut/offset-for {:year 2010 :month 7 :day 1 :hour 0  :tz :ny})))
  ;; (is (= 4 (sut/offset-for {:year 2010 :month 8 :day 1 :hour 0  :tz :ny})))
  ;; (is (= 4 (sut/offset-for {:year 2010 :month 9 :day 1 :hour 0  :tz :ny})))
  ;; (is (= 4 (sut/offset-for {:year 2010 :month 10 :day 1 :hour 0 :tz :ny})))
  ;; (is (= 4 (sut/offset-for {:year 2010 :month 11 :day 1 :hour 0 :tz :ny })))
  ;; (is (= 4 (sut/offset-for {:year 2010 :month 11 :day 7 :hour 0 :tz :ny })))
  ;; (is (= 5 (sut/offset-for {:year 2010 :month 11 :day 7 :hour 2 :tz :ny })))
  ;; (is (= 5 (sut/offset-for {:year 2010 :month 11 :day 20 :hour 0 :tz :ny})))
  ;; (is (= 5 (sut/offset-for {:year 2010 :month 12 :day 1 :hour 0 :tz :ny})))
  )

(deftest test-timezones
  (matcho/match
   (sut/day-saving :ny 2017)
   {:in  {:month 3 :day 12 :hour 2 :min 0}
    :out {:month 11 :day 5 :hour 2}})

  (matcho/match
   (sut/day-saving-with-utc :ny 2017)
   {:in     {:month 3 :day 12 :hour 2 :min 0}
    :in-utc {:month 3 :day 12 :hour 7}

    :out     {:month 11 :day 5 :hour 2}
    :out-utc {:month 11 :day 5 :hour 6}})

  (matcho/match
   (sut/day-saving-with-utc :ny 2018)
   {:in  {:month 3 :day 11}
    :out {:month 11 :day 4}})

  (matcho/match
   (sut/day-saving-with-utc :ny 2019)
   {:in  {:month 3 :day 10}
    :out {:month 11 :day 3}})

  (matcho/match
   (sut/to-utc {:year 2018 :month 5 :day 2 :hour 14 :tz :ny})
   {:year 2018 :month 5 :day 2 :hour 18})

  (matcho/match
   (sut/to-tz {:year 2018 :month 5 :day 2 :hour 18} :ny)
   {:year 2018 :month 5 :day 2 :hour 14 :tz :ny})


  (matcho/match
   (sut/to-utc {:year 2018 :month 2 :day 2 :hour 14 :tz :ny})
   {:year 2018 :month 2 :day 2 :hour 19})

  (matcho/match
   (sut/to-tz {:year 2018 :month 2 :day 2 :hour 19} :ny)
   {:year 2018 :month 2 :day 2 :hour 14 :tz :ny}))
