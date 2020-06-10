(ns chrono.ops-test
  (:require [chrono.ops :as sut]
            [matcho.core :as matcho]
            [clojure.test :refer :all]))


(deftest days-and-months
  (is (= [2011 4 10] (sut/days-and-months 2011 1 100)))
  (is (= [2011 2 1] (sut/days-and-months 2011 1 32)))

  (is (= [2012 1 1] (sut/days-and-months 2011 1 366)))
  (is (= [2013 1 1] (sut/days-and-months 2012 1 367)))

  (is (= [2012 12 31] (sut/days-and-months 2013 1 0)))
  (is (= [2012 12 30] (sut/days-and-months 2013 1 -1)))

  (is (= [2013 1 1] (sut/days-and-months 2013 1 1)))
  (is (= [2013 1 31] (sut/days-and-months 2013 1 31)))
  (is (= [2013 2 1] (sut/days-and-months 2013 1 32)))

  (is (= [2012 1 1] (sut/days-and-months 2013 1 -365))))

(deftest tz-operations-test
  (testing "to-utc"
    (is (= {:hour 10 :min 10 :tz 0}
           (sut/to-utc {:hour 10 :min 10})))

    (is (= {:hour 10 :min 10 :tz 0}
           (sut/to-utc {:hour 13 :min 10 :tz 3})))

    (is (= {:hour 10 :min 10 :tz 0}
           (sut/to-utc {:hour 7 :min 10 :tz -3})))

    (is (= {:min 10 :tz 0}
           (sut/to-utc {:min 130 :tz 2})))

    (is (= {:day -1 :hour 23 :tz 0}
           (sut/to-utc {:hour 1 :tz 2})))

    (is (= {:day 1 :hour 1 :tz 0}
           (sut/to-utc {:hour 23 :tz -2})))

    (is (= {:tz 0}
           (sut/to-utc {:hour 1 :tz 1}))))

  (testing "to-tz"
    (is (= (sut/to-tz {:hour 10 :min 10} nil)
           {:hour 10 :min 10}))

    (is (= (sut/to-tz {:hour 10 :min 10 :tz 0} 3)
           {:hour 13 :min 10 :tz 3}))
    (is (= (sut/to-tz {:hour 10 :min 10} 3)
           {:hour 10 :min 10 :tz 3}))

    (is (= (sut/to-tz {:hour 10 :min 10 :tz 0} -3)
           {:hour 7 :min 10 :tz -3}))
    (is (= (sut/to-tz {:hour 10 :min 10} -3)
           {:hour 10 :min 10 :tz -3}))

    (is (= (sut/to-tz {:min 10 :tz 0} 2)
           {:hour 2 :min 10 :tz 2}))
    (is (= (sut/to-tz {:min 10} 2)
           {:min 10 :tz 2}))

    (is (= (sut/to-tz {:day 1 :hour 1 :tz 0} -2)
           {:hour 23 :tz -2}))
    (is (= (sut/to-tz {:day 1 :hour 1} -2)
           {:day 1 :hour 1 :tz -2}))

    (is (= (sut/to-tz {:hour 1 :tz 0} -1)
           {:tz -1}))
    (is (= (sut/to-tz {:hour 1} -1)
           {:hour 1 :tz -1}))))

(deftest comparsion-operators-test
  (testing "="
    (is (sut/eq? {:year 2011 :month 1 :day 1 :hour 0}))
    (is (not (sut/eq? {:year 2011 :month 1 :day 2 :hour 0}
                      {:year 2011 :month 1 :day 1 :hour 0})))
    (is (sut/eq? {:year 2011 :month 1 :day 1 :hour 1}
                 {:year 2011 :month 1 :day 1 :hour 1}))
    (is (not (sut/eq? {:year 2011 :month 1 :day 1 :hour 0}
                      {:year 2011 :month 1 :day 2 :hour 0}
                      {:year 2011 :month 1 :day 1 :hour 0})))
    (is (sut/eq? {:year 2011 :month 1 :day 1 :hour 0}
                 {:year 2011 :month 1 :day 1 :hour 0}
                 {:year 2011 :month 1 :day 1 :hour 0})))
  (testing "= with utc-offset"
    (is (sut/eq? {:hour 10 :min 10} {:hour 13 :min 10 :tz 3}))
    (is (not (sut/eq? {:hour 13 :min 10} {:hour 13 :min 10 :tz 3})))
    (is (sut/eq? {:hour 210 :min 10} {:hour 213 :min 10 :tz 3}))
    (is (sut/eq? {:hour 15 :min 10} {:hour 13 :min 10 :tz -2}))
    (is (sut/eq? {:hour 12 :min 10 :tz 2} {:hour 13 :min 10 :tz 3}))
    (is (sut/eq? {:hour 10 :min 10} {:hour 12 :min 10 :tz 2})))
  (testing "not="
    (is (not (sut/not-eq? {:year 2011 :month 1 :day 1 :hour 0})))
    (is (not (sut/not-eq? {:year 2011 :month 1 :day 1 :hour 0}
                          {:year 2011 :month 1 :day 1 :hour 0})))
    (is (sut/not-eq? {:year 2011 :month 1 :day 2 :hour 1}
                     {:year 2011 :month 1 :day 1 :hour 1}))
    (is (not (sut/not-eq? {:year 2011 :month 1 :day 1 :hour 0}
                          {:year 2011 :month 1 :day 1 :hour 0}
                          {:year 2011 :month 1 :day 1 :hour 0})))
    (is (sut/not-eq? {:year 2011 :month 1 :day 1 :hour 0}
                     {:year 2011 :month 1 :day 2 :hour 0}
                     {:year 2011 :month 1 :day 1 :hour 0})))
  (testing "not= with utc-offset"
    (is (not (sut/not-eq? {:hour 10 :min 10} {:hour 13 :min 10 :tz 3})))
    (is (sut/not-eq? {:hour 13 :min 10} {:hour 13 :min 10 :tz 3}))
    (is (not (sut/not-eq? {:hour 210 :min 10} {:hour 213 :min 10 :tz 3})))
    (is (not (sut/not-eq? {:hour 15 :min 10} {:hour 13 :min 10 :tz -2})))
    (is (not (sut/not-eq? {:hour 12 :min 10 :tz 2} {:hour 13 :min 10 :tz 3})))
    (is (not (sut/not-eq? {:hour 10 :min 10} {:hour 12 :min 10 :tz 2}))))
  (testing ">"
    (is (sut/gt {:year 2011 :month 1 :day 1 :hour 0}))
    (is (sut/gt {:min 120} {:hour 1}))
    (is (not (sut/gt {:year 2011 :month 1 :day 1 :hour 0}
                     {:year 2011 :month 1 :day 2 :hour 0})))
    (is (sut/gt {:year 2011 :month 1 :day 2 :hour 0}
                {:year 2011 :month 1 :day 1 :hour 0}))
    (is (sut/gt {:year 2011 :month 1 :day 2 :hour 0}
                {:year 2011 :month 1 :day 1 :hour 1}
                {:year 2011 :month 1 :day 1 :hour 0})))
  (testing "> with utc-offset"
    (is (sut/gt {:hour 13} {:hour 13 :tz 3}))
    (is (not (sut/gt {:hour 13 :tz 3} {:hour 13})))
    (is (sut/gt {:hour 13 :tz -3} {:hour 13}))
    (is (not (sut/gt {:hour 13} {:hour 13 :tz -3})))
    (is (sut/gt {:hour 13 :tz -3} {:hour 13 :tz -2}))
    (is (not (sut/gt {:hour 13 :tz -2} {:hour 13 :tz -3})))
    (is (not (sut/gt {:hour 10 :min 10} {:hour 13 :min 10 :tz 3}))))
  (testing "<"
    (is (sut/lt {:year 2011 :month 1 :day 1 :hour 0}))
    (is (sut/lt {:hour 1} {:min 120}))
    (is (not (sut/lt {:year 2011 :month 1 :day 2 :hour 0}
                     {:year 2011 :month 1 :day 1 :hour 0})))
    (is (sut/lt {:year 2011 :month 1 :day 1 :hour 0}
                {:year 2011 :month 1 :day 2 :hour 0}))
    (is (sut/lt {:year 2011 :month 1 :day 1 :hour 0}
                {:year 2011 :month 1 :day 1 :hour 1}
                {:year 2011 :month 1 :day 2 :hour 0})))
  (testing "< with utc-offset"
    (is (sut/lt {:hour 13 :tz 3} {:hour 13}))
    (is (not (sut/lt {:hour 13} {:hour 13 :tz 3})))
    (is (sut/lt {:hour 13} {:hour 13 :tz -3}))
    (is (not (sut/lt {:hour 13 :tz -3} {:hour 13})))
    (is (sut/lt {:hour 13 :tz -2} {:hour 13 :tz -3}))
    (is (not (sut/lt {:hour 13 :tz -3} {:hour 13 :tz -2})))
    (is (not (sut/lt {:hour 13 :min 10 :tz 3} {:hour 10 :min 10}))))
  (testing "<="
    (is (sut/lte {:year 2011 :month 1 :day 1 :hour 0}))
    (is (sut/lte {:year 2011 :month 1 :day 1 :hour 0}
                 {:year 2011 :month 1 :day 2 :hour 0}))
    (is (sut/lte {:year 2011 :month 1 :day 1 :hour 0}
                 {:year 2011 :month 1 :day 1 :hour 0}))
    (is (not (sut/lte {:year 2011 :month 1 :day 2 :hour 0}
                      {:year 2011 :month 1 :day 1 :hour 0})))
    (is (sut/lte {:year 2011 :month 1 :day 1 :hour 0}
                 {:year 2011 :month 1 :day 1 :hour 0}
                 {:year 2011 :month 1 :day 2 :hour 0})))
  (testing ">="
    (is (sut/gte {:year 2011 :month 1 :day 1 :hour 0}))
    (is (sut/gte {:year 2011 :month 1 :day 2 :hour 0}
                 {:year 2011 :month 1 :day 1 :hour 0}))
    (is (sut/gte {:year 2011 :month 1 :day 2 :hour 1}
                 {:year 2011 :month 1 :day 1 :hour 1}
                 {:year 2011 :month 1 :day 1 :hour 0}))
    (is (not (sut/gte {:year 2011 :month 1 :day 1 :hour 0}
                      {:year 2011 :month 1 :day 2 :hour 0})))
    (is (sut/gte {:year 2011 :month 1 :day 2 :hour 0}
                 {:year 2011 :month 1 :day 1 :hour 0}
                 {:year 2011 :month 1 :day 1 :hour 0}))))

(deftest arithmetic-operations-test
  (testing "+"
    (def t
      {:year  2018
       :month 1
       :day   1
       :hour  12
       :min   30
       :sec   30
       :ms    500})

    (matcho/match (sut/plus t {:ms 200})
                  {:ms 700})

    (matcho/match (sut/plus t {:ms 600})
                  {:ms  100
                   :sec 31})

    (is (= (sut/plus {:ms 600} {:ms 600} {:ms 300})
           {:ms  500
            :sec 1}))

    (matcho/match (sut/plus t {:sec 20})
                  {:sec 50})

    (matcho/match (sut/plus t {:sec 20})
                  {:sec 50})

    (matcho/match (sut/plus t {:min 20})
                  {:hour 12
                   :min  50})

    (matcho/match (sut/plus t {:min 30})
                  {:hour 13})

    (is (= (sut/plus {:year 2018 :month 12 :day 31} {:day 1})
           {:year 2019 :month 1 :day 1}))

    (is (= (sut/plus {:year 2018 :month 1 :day 1} {:day 31})
           {:year 2018 :month 2 :day 1}))

    (is (= (sut/plus {:year 2018 :month 12 :day 31} {:day 366})
           {:year 2020 :month 1 :day 1}))

    (is (= (sut/plus {:year 2018 :month 2 :day 28} {:day 1})
           {:year 2018 :month 3 :day 1}))

    (is (= (sut/plus {:year 2018 :month 3 :day 30} {:day 1})
           {:year 2018 :month 3 :day 31}))

    (is (= (sut/plus {:year 2018 :month 3 :day 31} {:day 1})
           {:year 2018 :month 4 :day 1}))

    (is (= (sut/plus {:ms 100} {:ms 300})
           {:ms 400}))

    (is (= (sut/plus {:ms 900} {:ms 300})
           {:ms 200 :sec 1}))

    (is (= (sut/plus {:sec 40} {:sec 50})
           {:sec 30 :min 1}))

    (is (= (sut/plus {:min 40} {:min 50})
           {:min 30 :hour 1}))

    (is (= (sut/plus {:hour 13} {:hour 14})
           {:hour 3 :day 1}))

    (is (= (sut/plus {:year 2011 :month 1 :day 1 :hour 23} {:hour 5})
           {:year 2011 :month 1 :day 2 :hour 4}))

    (is (= (sut/plus {:year 2011 :month 1 :day 30} {:day 3})
           {:year 2011 :month 2 :day 2}))

    (is (= (sut/plus {:year 2011 :month 1 :day 1} {:day 365})
           {:year 2012 :month 1 :day 1}))

    (is (= (sut/plus {:year 2011 :month 12 :day 31 :hour 23} {:hour 5})
           {:year 2012 :month 1 :day 1 :hour 4}))

    (is (= (sut/plus {:year 2011 :month 1 :day 1 :hour 0} {:hour -1})
           {:year 2010 :month 12 :day 31 :hour 23}))

    (is (= (sut/plus {:year 2011 :month 1 :day 1 :hour 0} {:sec -1})
           {:year 2010 :month 12 :day 31 :hour 23 :min 59 :sec 59}))

    (is (= (sut/plus {:year 2011 :month 1 :day 1 :hour 0} {:ms -1})
           {:year 2010 :month 12 :day 31 :hour 23 :min 59 :sec 59 :ms 999}))

    (is (= (sut/plus {:year 2011 :month 1 :day 1 :hour 23} {:hour -23 :min -30})
           {:year 2010 :month 12 :day 31 :hour 23 :min 30}))

    (is (= (sut/plus {:year 2019 :month 11 :day 1} {:month 1})
           {:year 2019 :month 12 :day 1}))

    (is (= (sut/plus {:year 2019 :month 11 :day 1} {:month 2})
           {:year 2020 :month 1 :day 1}))

    (is (= (sut/plus {:year 2019 :month 12 :day 1} {:month 1})
           {:year 2020 :month 1 :day 1}))

    (is (= (sut/plus {:year 2019 :month 11 :day 32} {:month 1})
           {:year 2020 :month 1 :day 1}))

    (is (= {:year 2020 :month 2}
           (sut/plus {:year 2020 :month 2} {:day 0})))

    (is (= (sut/plus {:year 2019, :month 12, :day 10, :hour 13, :min 17, :sec 50, :ms 911} {:hour 2})
           {:year 2019, :month 12, :day 10, :hour 15, :min 17, :sec 50, :ms 911}))

    (is (= (sut/plus {:hour 4 :tz 2} {:hour 10})
           {:hour 14 :tz 2}))

    (is (= (sut/plus {:hour 23 :tz -2} {:hour 1})
           {:day 1 :tz -2}))

    (is (= (sut/plus {:hour 1 :tz -2} {:hour 1})
           {:hour 2 :tz -2}))

    (testing "with custom units"
      (def normalize-ns (sut/gen-norm :ns :ms 1000000 0 0))
      (defmethod sut/normalize-rule :ns [_ t] (normalize-ns t))

      (is (= (sut/plus {:ns 10} {:ns 1})
             {:ns 11}))

      (is (= (sut/plus {:ns 999999999} {:ns 1})
             {:sec 1}))

      (is (= (sut/plus {:ns 9999999} {:ns 999000001})
             {:sec 1 :ms 9}))

      (is (= (sut/plus {:year 2019 :month 12 :day 31 :hour 23 :min 59 :sec 59 :ns 999999999} {:ns 1})
             {:year 2020 :month 1 :day 1}))))

  (testing "-"
    (is (= (sut/minus {:year 2016 :month 12 :day 31 :hour 23 :min 30} {:day 365})
           {:year 2016, :month 1, :day 1, :hour 23, :min 30}))

    (is (= (sut/minus {:year 2016 :month 12 :day 31 :hour 23 :min 30} {:day 366})
           {:year 2015, :month 12, :day 31, :hour 23, :min 30}))

    (is (= {:year 2020 :month 1 :day 31}
           (sut/minus {:year 2020 :month 2}
                      {:day 1})))
    (is (= {:year 2020 :month 2}
           (sut/minus {:year 2020 :month 2}
                      {:day 0})))

    (is (= (sut/minus {:hour 2 :tz -2} {:hour 2})
           {:tz -2}))
    (is (= (sut/minus {:hour 3 :tz 2} {:hour 1 :tz 2})
           {:hour 2 :tz 2}))))

(deftest normalize-test
  (is (= {:year 2019 :month 11 :day 10}
         (sut/normalize {:year 2019 :month 11 :day 10})))
  (is (= {:year 2019 :month 12 :day 10}
         (sut/normalize {:year 2019 :month 12 :day 10})))
  (is (= {:year 2020 :month 12 :day 10}
         (sut/normalize {:year 2019 :month 24 :day 10})))
  (is (= {:year 2021 :month 1 :day 10}
         (sut/normalize {:year 2019 :month 25 :day 10}))))

(deftest test-timezones
  (testing "tz comparsion"
    (is (sut/lte {:year 2018 :month 5 :day 2 :hour 14 :tz :ny}
                 {:year 2018 :month 5 :day 2 :hour 14 :tz :ny}))

    (is (sut/lte {:year 2018 :month 5 :day 2 :hour 14 :tz :ny}
                 {:year 2018 :month 5 :day 2 :hour 14 :sec 1 :tz :ny}))

    (is (sut/gt {:year 2018 :month 5 :day 2 :hour 14 :sec 1 :tz :ny}
                {:year 2018 :month 5 :day 2 :hour 14 :tz :ny}))

    (is (sut/gt {:year 2018 :month 5 :day 2 :hour 13 :min 120 :tz :ny}
                {:year 2018 :month 5 :day 2 :hour 14 :tz :ny}))

    (is (sut/lte {:year 2018 :month 5 :day 2 :hour 14 :tz :ny}
                 {:year 2018 :month 5 :day 2 :hour 13 :min 120 :tz :ny}))

    (is (sut/lte {:year 2018 :month 5 :day 2 :hour 14 :tz :ny}
                 {:year 2018 :month 11}))

    (is (sut/gt {:year 2018 :month 11}
                {:year 2018 :month 5 :day 2 :hour 14 :tz :ny})))

  (matcho/match (sut/day-saving :ny 2017)
                {:in  {:month 3 :day 12 :hour 2 :min 0}
                 :out {:month 11 :day 5 :hour 2}})

  (matcho/match (sut/day-saving-with-utc :ny 2017)
                {:in     {:month 3 :day 12 :hour 2 :min 0}
                 :in-utc {:month 3 :day 12 :hour 7}

                 :out     {:month 11 :day 5 :hour 2}
                 :out-utc {:month 11 :day 5 :hour 6}})

  (matcho/match (sut/day-saving-with-utc :ny 2018)
                {:in  {:month 3 :day 11}
                 :out {:month 11 :day 4}})

  (matcho/match (sut/day-saving-with-utc :ny 2019)
                {:in  {:month 3 :day 10}
                 :out {:month 11 :day 3}})

  (is (= (sut/to-utc {:year 2018 :month 5 :day 2 :hour 14 :tz :ny})
         {:year 2018 :month 5 :day 2 :hour 18 :tz 0}))

  (is (= (sut/to-tz {:year 2018 :month 5 :day 2 :hour 18 :tz 0} :ny)
         {:year 2018 :month 5 :day 2 :hour 14 :tz :ny}))

  (is (= (sut/to-tz {:year 2018 :month 5 :day 2 :hour 18} :ny)
         {:year 2018 :month 5 :day 2 :hour 18 :tz :ny}))

  (is (= (sut/to-utc {:year 2018 :month 2 :day 2 :hour 14 :tz :ny})
         {:year 2018 :month 2 :day 2 :hour 19 :tz 0}))

  (is (= (sut/to-tz {:year 2018 :month 2 :day 2 :hour 19 :tz 0} :ny)
         {:year 2018 :month 2 :day 2 :hour 14 :tz :ny}))

  (is (= (sut/to-tz {:year 2018 :month 2 :day 2 :hour 19} :ny)
         {:year 2018 :month 2 :day 2 :hour 19 :tz :ny})))
