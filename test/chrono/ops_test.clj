(ns chrono.ops-test
  (:require [chrono.datetime :as cd]
            [chrono.interval :as ci]
            [chrono.ops :as sut]
            [clojure.test :refer :all]
            [matcho.core :as matcho]))

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

;; (deftest tz-operations-test
;;   (testing "to-utc"
;;     (is (= {:hour 10 :min 10 :tz 0}
;;            (sut/to-utc {:hour 10 :min 10})))

;;     (is (= {:hour 10 :min 10 :tz 0}
;;            (sut/to-utc {:hour 13 :min 10 :tz 3})))

;;     (is (= {:hour 10 :min 10 :tz 0}
;;            (sut/to-utc {:hour 7 :min 10 :tz -3})))

;;     (is (= {:min 10 :tz 0}
;;            (sut/to-utc {:min 130 :tz 2})))

;;     (is (= {:day -1 :hour 23 :tz 0}
;;            (sut/to-utc {:hour 1 :tz 2})))

;;     (is (= {:day 1 :hour 1 :tz 0}
;;            (sut/to-utc {:hour 23 :tz -2})))

;;     (is (= {:tz 0}
;;            (sut/to-utc {:hour 1 :tz 1}))))

;;   (testing "to-tz"
;;     (is (= (sut/to-tz {:hour 10 :min 10} nil)
;;            {:hour 10 :min 10}))

;;     (is (= (sut/to-tz {:hour 10 :min 10 :tz 0} 3)
;;            {:hour 13 :min 10 :tz 3}))
;;     (is (= (sut/to-tz {:hour 10 :min 10} 3)
;;            {:hour 10 :min 10 :tz 3}))

;;     (is (= (sut/to-tz {:hour 10 :min 10 :tz 0} -3)
;;            {:hour 7 :min 10 :tz -3}))
;;     (is (= (sut/to-tz {:hour 10 :min 10} -3)
;;            {:hour 10 :min 10 :tz -3}))

;;     (is (= (sut/to-tz {:min 10 :tz 0} 2)
;;            {:hour 2 :min 10 :tz 2}))
;;     (is (= (sut/to-tz {:min 10} 2)
;;            {:min 10 :tz 2}))

;;     (is (= (sut/to-tz {:day 1 :hour 1 :tz 0} -2)
;;            {:hour 23 :tz -2}))
;;     (is (= (sut/to-tz {:day 1 :hour 1} -2)
;;            {:day 1 :hour 1 :tz -2}))

;;     (is (= (sut/to-tz {:hour 1 :tz 0} -1)
;;            {:tz -1}))
;;     (is (= (sut/to-tz {:hour 1} -1)
;;            {:hour 1 :tz -1}))))

;; (deftest comparison-operators-test
;;   (testing "="
;;     (is (sut/eq? {:year 2011 :month 1 :day 1 :hour 0}))
;;     (is (not (sut/eq? {:year 2011 :month 1 :day 2 :hour 0}
;;                       {:year 2011 :month 1 :day 1 :hour 0})))
;;     (is (sut/eq? {:year 2011 :month 1 :day 1 :hour 1}
;;                  {:year 2011 :month 1 :day 1 :hour 1}))
;;     (is (not (sut/eq? {:year 2011 :month 1 :day 1 :hour 0}
;;                       {:year 2011 :month 1 :day 2 :hour 0}
;;                       {:year 2011 :month 1 :day 1 :hour 0})))
;;     (is (sut/eq? {:year 2011 :month 1 :day 1 :hour 0}
;;                  {:year 2011 :month 1 :day 1 :hour 0}
;;                  {:year 2011 :month 1 :day 1 :hour 0})))
;;   (testing "= with utc-offset"
;;     (is (sut/eq? {:hour 10 :min 10} {:hour 13 :min 10 :tz 3}))
;;     (is (not (sut/eq? {:hour 13 :min 10} {:hour 13 :min 10 :tz 3})))
;;     (is (sut/eq? {:hour 210 :min 10} {:hour 213 :min 10 :tz 3}))
;;     (is (sut/eq? {:hour 15 :min 10} {:hour 13 :min 10 :tz -2}))
;;     (is (sut/eq? {:hour 12 :min 10 :tz 2} {:hour 13 :min 10 :tz 3}))
;;     (is (sut/eq? {:hour 10 :min 10} {:hour 12 :min 10 :tz 2})))
;;   (testing "not="
;;     (is (not (sut/not-eq? {:year 2011 :month 1 :day 1 :hour 0})))
;;     (is (not (sut/not-eq? {:year 2011 :month 1 :day 1 :hour 0}
;;                           {:year 2011 :month 1 :day 1 :hour 0})))
;;     (is (sut/not-eq? {:year 2011 :month 1 :day 2 :hour 1}
;;                      {:year 2011 :month 1 :day 1 :hour 1}))
;;     (is (not (sut/not-eq? {:year 2011 :month 1 :day 1 :hour 0}
;;                           {:year 2011 :month 1 :day 1 :hour 0}
;;                           {:year 2011 :month 1 :day 1 :hour 0})))
;;     (is (sut/not-eq? {:year 2011 :month 1 :day 1 :hour 0}
;;                      {:year 2011 :month 1 :day 2 :hour 0}
;;                      {:year 2011 :month 1 :day 1 :hour 0})))
;;   (testing "not= with utc-offset"
;;     (is (not (sut/not-eq? {:hour 10 :min 10} {:hour 13 :min 10 :tz 3})))
;;     (is (sut/not-eq? {:hour 13 :min 10} {:hour 13 :min 10 :tz 3}))
;;     (is (not (sut/not-eq? {:hour 210 :min 10} {:hour 213 :min 10 :tz 3})))
;;     (is (not (sut/not-eq? {:hour 15 :min 10} {:hour 13 :min 10 :tz -2})))
;;     (is (not (sut/not-eq? {:hour 12 :min 10 :tz 2} {:hour 13 :min 10 :tz 3})))
;;     (is (not (sut/not-eq? {:hour 10 :min 10} {:hour 12 :min 10 :tz 2}))))
;;   (testing ">"
;;     (is (sut/gt {:year 2011 :month 1 :day 1 :hour 0}))
;;     (is (sut/gt {:min 120} {:hour 1}))
;;     (is (not (sut/gt {:year 2011 :month 1 :day 1 :hour 0}
;;                      {:year 2011 :month 1 :day 2 :hour 0})))
;;     (is (sut/gt {:year 2011 :month 1 :day 2 :hour 0}
;;                 {:year 2011 :month 1 :day 1 :hour 0}))
;;     (is (sut/gt {:year 2011 :month 1 :day 2 :hour 0}
;;                 {:year 2011 :month 1 :day 1 :hour 1}
;;                 {:year 2011 :month 1 :day 1 :hour 0})))
;;   (testing "> with utc-offset"
;;     (is (sut/gt {:hour 13} {:hour 13 :tz 3}))
;;     (is (not (sut/gt {:hour 13 :tz 3} {:hour 13})))
;;     (is (sut/gt {:hour 13 :tz -3} {:hour 13}))
;;     (is (not (sut/gt {:hour 13} {:hour 13 :tz -3})))
;;     (is (sut/gt {:hour 13 :tz -3} {:hour 13 :tz -2}))
;;     (is (not (sut/gt {:hour 13 :tz -2} {:hour 13 :tz -3})))
;;     (is (not (sut/gt {:hour 10 :min 10} {:hour 13 :min 10 :tz 3}))))
;;   (testing "<"
;;     (is (sut/lt {:year 2011 :month 1 :day 1 :hour 0}))
;;     (is (sut/lt {:hour 1} {:min 120}))
;;     (is (not (sut/lt {:year 2011 :month 1 :day 2 :hour 0}
;;                      {:year 2011 :month 1 :day 1 :hour 0})))
;;     (is (sut/lt {:year 2011 :month 1 :day 1 :hour 0}
;;                 {:year 2011 :month 1 :day 2 :hour 0}))
;;     (is (sut/lt {:year 2011 :month 1 :day 1 :hour 0}
;;                 {:year 2011 :month 1 :day 1 :hour 1}
;;                 {:year 2011 :month 1 :day 2 :hour 0})))
;;   (testing "< with utc-offset"
;;     (is (sut/lt {:hour 13 :tz 3} {:hour 13}))
;;     (is (not (sut/lt {:hour 13} {:hour 13 :tz 3})))
;;     (is (sut/lt {:hour 13} {:hour 13 :tz -3}))
;;     (is (not (sut/lt {:hour 13 :tz -3} {:hour 13})))
;;     (is (sut/lt {:hour 13 :tz -2} {:hour 13 :tz -3}))
;;     (is (not (sut/lt {:hour 13 :tz -3} {:hour 13 :tz -2})))
;;     (is (not (sut/lt {:hour 13 :min 10 :tz 3} {:hour 10 :min 10}))))
;;   (testing "<="
;;     (is (sut/lte {:year 2011 :month 1 :day 1 :hour 0}))
;;     (is (sut/lte {:year 2011 :month 1 :day 1 :hour 0}
;;                  {:year 2011 :month 1 :day 2 :hour 0}))
;;     (is (sut/lte {:year 2011 :month 1 :day 1 :hour 0}
;;                  {:year 2011 :month 1 :day 1 :hour 0}))
;;     (is (not (sut/lte {:year 2011 :month 1 :day 2 :hour 0}
;;                       {:year 2011 :month 1 :day 1 :hour 0})))
;;     (is (sut/lte {:year 2011 :month 1 :day 1 :hour 0}
;;                  {:year 2011 :month 1 :day 1 :hour 0}
;;                  {:year 2011 :month 1 :day 2 :hour 0})))
;;   (testing ">="
;;     (is (sut/gte {:year 2011 :month 1 :day 1 :hour 0}))
;;     (is (sut/gte {:year 2011 :month 1 :day 2 :hour 0}
;;                  {:year 2011 :month 1 :day 1 :hour 0}))
;;     (is (sut/gte {:year 2011 :month 1 :day 2 :hour 1}
;;                  {:year 2011 :month 1 :day 1 :hour 1}
;;                  {:year 2011 :month 1 :day 1 :hour 0}))
;;     (is (not (sut/gte {:year 2011 :month 1 :day 1 :hour 0}
;;                       {:year 2011 :month 1 :day 2 :hour 0})))
;;     (is (sut/gte {:year 2011 :month 1 :day 2 :hour 0}
;;                  {:year 2011 :month 1 :day 1 :hour 0}
;;                  {:year 2011 :month 1 :day 1 :hour 0}))))

(deftest arithmetic-operations-test
  (testing "+"
    (def t
      {::cd/year  2018
       ::cd/month 1
       ::cd/day   1
       ::cd/hour  12
       ::cd/min   30
       ::cd/sec   30
       ::cd/ms    500})

    (matcho/match (sut/plus t {::ci/ms 200})
                  {::cd/ms 700})

    (matcho/match (sut/plus t {::ci/ms 600})
                  {::cd/ms  100
                   ::cd/sec 31})

    (is (= {::ci/ms  500
            ::ci/sec 1}
           (sut/plus {::ci/ms 600} {::ci/ms 600} {::ci/ms 300})))

    (matcho/match (sut/plus t {::ci/sec 20})
                  {::cd/sec 50})

    (matcho/match (sut/plus t {::ci/sec 20})
                  {::cd/sec 50})

    (matcho/match (sut/plus t {::ci/min 20})
                  {::cd/hour 12
                   ::cd/min  50})

    (matcho/match (sut/plus t {::ci/min 30})
                  {::cd/hour 13})

    (is (= #::cd{:year 2019 :month 1 :day 1}
           (sut/plus #::cd{:year 2018 :month 12 :day 31}
                     #::ci{:day 1})))

    (is (= #::cd{:year 2018 :month 2 :day 1}
           (sut/plus #::cd{:year 2018 :month 1 :day 1}
                     #::ci{:day 31})))

    (is (= #::cd{:year 2020 :month 1 :day 1}
           (sut/plus #::cd{:year 2018 :month 12 :day 31}
                     #::ci{:day 366})))

    (is (= #::cd{:year 2018 :month 3 :day 1}
           (sut/plus #::cd{:year 2018 :month 2 :day 28}
                     #::ci{:day 1})))

    (is (= #::cd{:year 2018 :month 3 :day 31}
           (sut/plus #::cd{:year 2018 :month 3 :day 30}
                     #::ci{:day 1})))

    (is (= #::cd{:year 2018 :month 4 :day 1}
           (sut/plus #::cd{:year 2018 :month 3 :day 31}
                     #::ci{:day 1})))

    (is (= #::ci{:ms 400}
           (sut/plus #::ci{:ms 100} #::ci{:ms 300})))

    (is (= #::ci{:ms 200 :sec 1}
           (sut/plus #::ci{:ms 900} #::ci{:ms 300})))

    (is (= #::ci{:sec 30 :min 1}
           (sut/plus #::ci{:sec 40} #::ci{:sec 50})))

    (is (= #::ci{:min 30 :hour 1}
           (sut/plus #::ci{:min 40} #::ci{:min 50})))

    (is (= #::ci{:hour 3 :day 1}
           (sut/plus #::ci{:hour 13} #::ci{:hour 14})))

    (is (= #::cd{:year 2011 :month 1 :day 2 :hour 4}
           (sut/plus #::cd{:year 2011 :month 1 :day 1 :hour 23}
                     #::ci{:hour 5})))

    (is (= #::cd{:year 2011 :month 2 :day 2}
           (sut/plus #::cd{:year 2011 :month 1 :day 30}
                     #::ci{:day 3})))

    (is (= #::cd{:year 2012 :month 1 :day 1}
           (sut/plus #::cd{:year 2011 :month 1 :day 1}
                     #::ci{:day 365})))

    (is (= #::cd{:year 2012 :month 1 :day 1 :hour 4}
           (sut/plus #::cd{:year 2011 :month 12 :day 31 :hour 23}
                     #::ci{:hour 5})))

    (is (= #::cd{:year 2010 :month 12 :day 31 :hour 23}
           (sut/plus #::cd{:year 2011 :month 1 :day 1 :hour 0}
                     #::ci{:hour -1})))

    (is (= #::cd{:year 2010 :month 12 :day 31 :hour 23 :min 59 :sec 59}
           (sut/plus #::cd{:year 2011 :month 1 :day 1 :hour 0}
                     #::ci{:sec -1})))

    (is (= #::cd{:year 2010 :month 12 :day 31 :hour 23 :min 59 :sec 59 :ms 999}
           (sut/plus #::cd{:year 2011 :month 1 :day 1 :hour 0}
                     #::ci{:ms -1})))

    (is (= #::cd{:year 2010 :month 12 :day 31 :hour 23 :min 30}
           (sut/plus #::cd{:year 2011 :month 1 :day 1 :hour 23}
                     #::ci{:hour -23 :min -30})))

    (is (= #::cd{:year 2019 :month 12 :day 1}
           (sut/plus #::cd{:year 2019 :month 11 :day 1}
                     #::ci{:month 1})))

    (is (= #::cd{:year 2020 :month 1 :day 1}
           (sut/plus #::cd{:year 2019 :month 11 :day 1}
                     #::ci{:month 2})))

    (is (= #::cd{:year 2020 :month 1 :day 1}
           (sut/plus #::cd{:year 2019 :month 12 :day 1}
                     #::ci{:month 1})))

    (is (= #::cd{:year 2020 :month 1 :day 1}
           (sut/plus #::cd{:year 2019 :month 11 :day 32}
                     #::ci{:month 1})))

    (is (= #::cd{:year 2019, :month 12, :day 10, :hour 15, :min 17, :sec 50, :ms 911}
           (sut/plus #::cd{:year 2019, :month 12, :day 10, :hour 13, :min 17, :sec 50, :ms 911}
                     #::ci{:hour 2})))

    (is (= #::cd{:hour 14 :tz 2}
           (sut/plus #::cd{:hour 4 :tz 2} #::ci{:hour 10})))

    (is (= #::cd{:day 1 :tz -2}
           (sut/plus #::cd{:hour 23 :tz -2} #::ci{:hour 1})))

    (is (= #::cd{:hour 2 :tz -2}
           (sut/plus #::cd{:hour 1 :tz -2} #::ci{:hour 1 :tz 3})))

    (is (= #::cd{:hour 2 :tz -2}
           (sut/plus #::ci{:hour 1 :tz 3} #::cd{:hour 1 :tz -2})))

    (testing "with custom units"
      (def normalize-cd-ns (sut/gen-norm ::cd/ns ::cd/ms 1000000 0))
      (def normalize-ci-ns (sut/gen-norm ::ci/ns ::ci/ms 1000000 0))

      (defmethod sut/normalize-rule ::cd/ns [_ t] (normalize-cd-ns t))
      (defmethod sut/normalize-rule ::ci/ns [_ t] (normalize-ci-ns t))

      (is (= #::ci{:ns 11}
             (sut/plus #::ci{:ns 10} #::ci{:ns 1})))

      (is (= #::ci{:sec 1}
             (sut/plus #::ci{:ns 999999999} #::ci{:ns 1})))

      (is (= #::ci{:sec 1 :ms 9}
             (sut/plus #::ci{:ns 9999999} #::ci{:ns 999000001})))

      (is (= #::cd{:year 2020 :month 1 :day 1}
             (sut/plus #::cd{:year 2019 :month 12 :day 31 :hour 23 :min 59 :sec 59 :ns 999999999}
                       #::ci{:ns 1})))))

  (testing "-"
    (is (= #::cd{:year 2016, :month 1, :day 1, :hour 23, :min 30}
           (sut/minus #::cd{:year 2016 :month 12 :day 31 :hour 23 :min 30}
                      #::ci{:day 365})))

    (is (= #::cd{:year 2015, :month 12, :day 31, :hour 23, :min 30}
           (sut/minus #::cd{:year 2016 :month 12 :day 31 :hour 23 :min 30}
                      #::ci{:day 366})))
    (is (= #::cd{:tz -2}
           (sut/minus #::cd{:hour 2 :tz -2} #::ci{:hour 2})))
    (is (= #::ci{:day 2 :hour 1}
           (sut/minus #::cd{:year 2020 :month 6 :day 7 :hour 15 :min 30 :tz 3}
                      #::cd{:year 2020 :month 6 :day 5 :hour 13 :min 30 :tz 2})))))

;; (deftest arithmetic-operations-test
;;   (testing "+"
;;     (def t
;;       #_{::ch/year  2018
;;        ::ch/month 1
;;        ::ch/day   1
;;        ::ch/hour  12
;;        ::ch/min   30
;;        ::ch/sec   30
;;        ::ch/ms    500})

;;     (matcho/match (sut/plus t {::ch/ms 200})
;;                   {::ch/ms 700})

;;     #_(matcho/match (sut/plus t {:ms 600})
;;                   {:ms  100
;;                    :sec 31})

;;     (is (= (sut/plus {:ms 600} {:ms 600} {:ms 300})
;;            {:ms  500
;;             :sec 1}))

;;     (matcho/match (sut/plus t {:sec 20})
;;                   {:sec 50})

;;     (matcho/match (sut/plus t {:sec 20})
;;                   {:sec 50})

;;     (matcho/match (sut/plus t {:min 20})
;;                   {:hour 12
;;                    :min  50})

;;     (matcho/match (sut/plus t {:min 30})
;;                   {:hour 13})

;;     (is (= (sut/plus {:year 2018 :month 12 :day 31} {:day 1})
;;            {:year 2019 :month 1 :day 1}))

;;     (is (= (sut/plus {:year 2018 :month 1 :day 1} {:day 31})
;;            {:year 2018 :month 2 :day 1}))

;;     (is (= (sut/plus {:year 2018 :month 12 :day 31} {:day 366})
;;            {:year 2020 :month 1 :day 1}))

;;     (is (= (sut/plus {:year 2018 :month 2 :day 28} {:day 1})
;;            {:year 2018 :month 3 :day 1}))

;;     (is (= (sut/plus {:year 2018 :month 3 :day 30} {:day 1})
;;            {:year 2018 :month 3 :day 31}))

;;     (is (= (sut/plus {:year 2018 :month 3 :day 31} {:day 1})
;;            {:year 2018 :month 4 :day 1}))

;;     (is (= (sut/plus {:ms 100} {:ms 300})
;;            {:ms 400}))

;;     (is (= (sut/plus {:ms 900} {:ms 300})
;;            {:ms 200 :sec 1}))

;;     (is (= (sut/plus {:sec 40} {:sec 50})
;;            {:sec 30 :min 1}))

;;     (is (= (sut/plus {:min 40} {:min 50})
;;            {:min 30 :hour 1}))

;;     (is (= (sut/plus {:hour 13} {:hour 14})
;;            {:hour 3 :day 1}))

;;     (is (= (sut/plus {:year 2011 :month 1 :day 1 :hour 23} {:hour 5})
;;            {:year 2011 :month 1 :day 2 :hour 4}))

;;     (is (= (sut/plus {:year 2011 :month 1 :day 30} {:day 3})
;;            {:year 2011 :month 2 :day 2}))

;;     (is (= (sut/plus {:year 2011 :month 1 :day 1} {:day 365})
;;            {:year 2012 :month 1 :day 1}))

;;     (is (= (sut/plus {:year 2011 :month 12 :day 31 :hour 23} {:hour 5})
;;            {:year 2012 :month 1 :day 1 :hour 4}))

;;     (is (= (sut/plus {:year 2011 :month 1 :day 1 :hour 0} {:hour -1})
;;            {:year 2010 :month 12 :day 31 :hour 23}))

;;     (is (= (sut/plus {:year 2011 :month 1 :day 1 :hour 0} {:sec -1})
;;            {:year 2010 :month 12 :day 31 :hour 23 :min 59 :sec 59}))

;;     (is (= (sut/plus {:year 2011 :month 1 :day 1 :hour 0} {:ms -1})
;;            {:year 2010 :month 12 :day 31 :hour 23 :min 59 :sec 59 :ms 999}))

;;     (is (= (sut/plus {:year 2011 :month 1 :day 1 :hour 23} {:hour -23 :min -30})
;;            {:year 2010 :month 12 :day 31 :hour 23 :min 30}))

;;     (is (= (sut/plus {:year 2019 :month 11 :day 1} {:month 1})
;;            {:year 2019 :month 12 :day 1}))

;;     (is (= (sut/plus {:year 2019 :month 11 :day 1} {:month 2})
;;            {:year 2020 :month 1 :day 1}))

;;     (is (= (sut/plus {:year 2019 :month 12 :day 1} {:month 1})
;;            {:year 2020 :month 1 :day 1}))

;;     (is (= (sut/plus {:year 2019 :month 11 :day 32} {:month 1})
;;            {:year 2020 :month 1 :day 1}))

;;     (is (= (sut/plus {:year 2019, :month 12, :day 10, :hour 13, :min 17, :sec 50, :ms 911} {:hour 2})
;;            {:year 2019, :month 12, :day 10, :hour 15, :min 17, :sec 50, :ms 911}))

;;     (is (= (sut/plus {:hour 4 :tz 2} {:hour 10})
;;            {:hour 14 :tz 2}))

;;     (is (= (sut/plus {:hour 23 :tz -2} {:hour 1})
;;            {:day 1 :tz -2}))

;;     (is (= (sut/plus {:hour 1 :tz -2} {:hour 1})
;;            {:hour 2 :tz -2}))

;;     (testing "with custom units"
;;       (def normalize-ns (sut/gen-norm :ns :ms 1000000 0))
;;       (defmethod sut/normalize-rule :ns [_ t] (normalize-ns t))

;;       (is (= (sut/plus {:ns 10} {:ns 1})
;;              {:ns 11}))

;;       (is (= (sut/plus {:ns 999999999} {:ns 1})
;;              {:sec 1}))

;;       (is (= (sut/plus {:ns 9999999} {:ns 999000001})
;;              {:sec 1 :ms 9}))

;;       (is (= (sut/plus {:year 2019 :month 12 :day 31 :hour 23 :min 59 :sec 59 :ns 999999999} {:ns 1})
;;              {:year 2020 :month 1 :day 1}))))

;;   (testing "-"
;;     (is (= (sut/minus {:year 2016 :month 12 :day 31 :hour 23 :min 30} {:day 365})
;;            {:year 2016, :month 1, :day 1, :hour 23, :min 30}))

;;     (is (= (sut/minus {:year 2016 :month 12 :day 31 :hour 23 :min 30} {:day 366})
;;            {:year 2015, :month 12, :day 31, :hour 23, :min 30}))
;;     (is (= (sut/minus {:hour 2 :tz -2} {:hour 2})
;;            {:tz -2}))
;;     (is (= (sut/minus {:hour 3 :tz 2} {:hour 1 :tz 2})
;;            {:hour 2 :tz 2}))))

(deftest test-timezones
  (testing "tz comparison"
    (is (sut/lte #::cd{:year 2018 :month 5 :day 2 :hour 14 :tz :ny}
                 #::cd{:year 2018 :month 5 :day 2 :hour 14 :tz :ny}))

    (is (sut/lte #::cd{:year 2018 :month 5 :day 2 :hour 14 :tz :ny}
                 #::cd{:year 2018 :month 5 :day 2 :hour 14 :sec 1 :tz :ny}))

    (is (sut/gt #::cd{:year 2018 :month 5 :day 2 :hour 14 :sec 1 :tz :ny}
                #::cd{:year 2018 :month 5 :day 2 :hour 14 :tz :ny}))

    (is (sut/gt #::cd{:year 2018 :month 5 :day 2 :hour 13 :min 120 :tz :ny}
                #::cd{:year 2018 :month 5 :day 2 :hour 14 :tz :ny}))

    (is (sut/lte #::cd{:year 2018 :month 5 :day 2 :hour 14 :tz :ny}
                 #::cd{:year 2018 :month 5 :day 2 :hour 13 :min 120 :tz :ny}))

    (is (sut/lte #::cd{:year 2018 :month 5 :day 2 :hour 14 :tz :ny}
                 #::cd{:year 2018 :month 11}))

    (is (sut/gt #::cd{:year 2018 :month 11}
                #::cd{:year 2018 :month 5 :day 2 :hour 14 :tz :ny})))

  (is (= #::cd{:year 2018 :month 5 :day 2 :hour 18 :tz 0}
         (sut/to-utc #::cd{:year 2018 :month 5 :day 2 :hour 14 :tz :ny})))

  (is (= #::cd{:year 2018 :month 5 :day 2 :hour 14 :tz :ny}
         (sut/to-tz #::cd{:year 2018 :month 5 :day 2 :hour 18 :tz 0} :ny)))

  (is (= #::cd{:year 2018 :month 5 :day 2 :hour 18 :tz :ny}
         (sut/to-tz #::cd{:year 2018 :month 5 :day 2 :hour 18} :ny)))

  (is (= #::cd{:year 2018 :month 2 :day 2 :hour 19 :tz 0}
         (sut/to-utc #::cd{:year 2018 :month 2 :day 2 :hour 14 :tz :ny})))

  (is (= #::cd{:year 2018 :month 2 :day 2 :hour 14 :tz :ny}
         (sut/to-tz #::cd{:year 2018 :month 2 :day 2 :hour 19 :tz 0} :ny)))

  (is (= #::cd{:year 2018 :month 2 :day 2 :hour 19 :tz :ny}
         (sut/to-tz #::cd{:year 2018 :month 2 :day 2 :hour 19} :ny))))
