(ns chrono.ops-test
  (:require [chrono.ops :as sut]
            [matcho.core :as matcho]
            [clojure.test :refer :all]))

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
  (testing ">"
    (is (sut/gt {:year 2011 :month 1 :day 1 :hour 0}))
    (is (not (sut/gt {:year 2011 :month 1 :day 1 :hour 0}
                     {:year 2011 :month 1 :day 2 :hour 0})))
    (is (sut/gt {:year 2011 :month 1 :day 2 :hour 0}
                {:year 2011 :month 1 :day 1 :hour 0}))
    (is (sut/gt {:year 2011 :month 1 :day 2 :hour 0}
                {:year 2011 :month 1 :day 1 :hour 1}
                {:year 2011 :month 1 :day 1 :hour 0})))
  (testing "<"
    (is (sut/lt {:year 2011 :month 1 :day 1 :hour 0}))
    (is (not (sut/lt {:year 2011 :month 1 :day 2 :hour 0}
                     {:year 2011 :month 1 :day 1 :hour 0})))
    (is (sut/lt {:year 2011 :month 1 :day 1 :hour 0}
                {:year 2011 :month 1 :day 2 :hour 0}))
    (is (sut/lt {:year 2011 :month 1 :day 1 :hour 0}
                {:year 2011 :month 1 :day 1 :hour 1}
                {:year 2011 :month 1 :day 2 :hour 0})))
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
                 {:year 2011 :month 1 :day 1 :hour 0})))

  (testing "tz comparsion"
    (is (sut/lte {:year 2018 :month 5 :day 2 :hour 14 :tz :ny}
                 {:year 2018 :month 5 :day 2 :hour 14 :tz :ny}))

    (is (sut/lte {:year 2018 :month 5 :day 2 :hour 14 :tz :ny}
                 {:year 2018 :month 5 :day 2 :hour 14 :sec 1 :tz :ny}))

    (is (sut/gt {:year 2018 :month 5 :day 2 :hour 14 :sec 1 :tz :ny}
                {:year 2018 :month 5 :day 2 :hour 14 :tz :ny}))

    (is (sut/lte {:year 2018 :month 5 :day 2 :hour 14 :tz :ny}
                 {:year 2018 :month 11}))

    (is (sut/gt {:year 2018 :month 11}
                {:year 2018 :month 5 :day 2 :hour 14 :tz :ny}))))

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

    (matcho/match
     (sut/plus t {:ms 200})
     {:ms 700})

    (matcho/match
     (sut/plus t {:ms 600})
     {:ms  100
      :sec 31})

    (matcho/match
     (sut/plus {:ms 600} {:ms 600} {:ms 300})
     {:ms  500
      :sec 1})

    (matcho/match
     (sut/plus t {:sec 20})
     {:sec 50})

    (matcho/match
     (sut/plus t {:sec 20})
     {:sec 50})

    (matcho/match
     (sut/plus t {:min 20})
     {:hour 12
      :min  50})

    (matcho/match
     (sut/plus t {:min 30})
     {:hour 13})

    (matcho/match
     (sut/plus {:year 2018 :month 12 :day 31} {:day 1})
     {:year 2019 :month 1 :day 1})

    (matcho/match
     (sut/plus {:year 2018 :month 1 :day 1} {:day 31})
     {:year 2018 :month 2 :day 1})

    (matcho/match
     (sut/plus {:year 2018 :month 12 :day 31} {:day 366})
     {:year 2020 :month 1 :day 1})

    (matcho/match
     (sut/plus {:year 2018 :month 2 :day 28} {:day 1})
     {:year 2018 :month 3 :day 1})

    (matcho/match
     (sut/plus {:year 2018 :month 3 :day 30} {:day 1})
     {:year 2018 :month 3 :day 31})

    (matcho/match
     (sut/plus {:year 2018 :month 3 :day 31} {:day 1})
     {:year 2018 :month 4 :day 1})


    (matcho/match
     (sut/plus {:ms 100} {:ms 300})
     {:ms 400})

    (matcho/match
     (sut/plus {:ms 900} {:ms 300})
     {:ms 200 :sec 1})

    (matcho/match
     (sut/plus {:sec 40} {:sec 50})
     {:sec 30 :min 1})

    (matcho/match
     (sut/plus {:min 40} {:min 50})
     {:min 30 :hour 1})

    (matcho/match
     (sut/plus {:hour 13} {:hour 14})
     {:hour 3 :day 1})

    (matcho/match
     (sut/plus {:year 2011 :month 1 :day 1 :hour 23} {:hour 5})
     {:year 2011 :month 1 :day 2 :hour 4})

    (matcho/match
     (sut/plus {:year 2011 :month 1 :day 30} {:day 3})
     {:year 2011 :month 2 :day 2})

    (matcho/match
     (sut/plus {:year 2011 :month 1 :day 1} {:day 365})
     {:year 2012 :month 1 :day 1})

    (matcho/match
     (sut/plus {:year 2011 :month 12 :day 31 :hour 23} {:hour 5})
     {:year 2012 :month 1 :day 1 :hour 4})

    (matcho/match
     (sut/plus {:year 2011 :month 1 :day 1 :hour 0} {:hour -1})
     {:year 2010 :month 12 :day 31 :hour 23})

    (matcho/match
     (sut/plus {:year 2011 :month 1 :day 1 :hour 0} {:sec -1})
     {:year 2010 :month 12 :day 31 :hour 23 :min 59 :sec 59})

    (matcho/match
     (sut/plus {:year 2011 :month 1 :day 1 :hour 0} {:ms -1})
     {:year 2010 :month 12 :day 31 :hour 23 :min 59 :sec 59 :ms 999})

    (matcho/match
     (sut/plus {:year 2011 :month 1 :day 1 :hour 23} {:hour -23 :min -30})
     {:year 2010 :month 12 :day 31 :hour 23 :min 30})))
