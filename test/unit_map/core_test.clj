(ns unit-map.core-test
  (:require [unit-map.core :as sut]
            [clojure.test :as t]))


(def treg_ (atom nil))


(do
  (defn leap-year? [{:keys [year]}]
    (and (zero? (rem year 4))
         (or (pos? (rem year 100))
             (zero? (rem year 400)))))


  (defn days-in-month [{:as date, :keys [month]}]
    (condp contains? month
      #{:jan :mar :may :jul :aug :oct :dec} 31
      #{:apr :jun :sep :nov}                30
      #{:feb}                               (if (leap-year? date) 29 28)
      ##Inf))

  (sut/reguseq! treg_ :ms   #unit-map/useq[0 1 .. 999] :next :sec)
  (sut/reguseq! treg_ :sec  #unit-map/useq[0 1 .. 59] :next :min)
  (sut/reguseq! treg_ :min  #unit-map/useq[0 1 .. 59] :next :hour)
  (sut/reguseq! treg_ :hour #unit-map/useq[0 1 .. 23] :next :day)

  (sut/reguseq! treg_ :day   #unit-map/useq[1 2 .. days-in-month] :next :month)
  (sut/reguseq! treg_ :month #unit-map/useq[:jan :feb  :mar :apr :may  :jun :jul :aug  :sep :oct :nov  :dec] :next :year)
  (sut/reguseq! treg_ :year  #unit-map/useq[##-Inf .. -2 -1 1 2 .. ##Inf])

  (sut/reguseq! treg_ :ms   #unit-map/useq[0 1 .. ##Inf])
  (sut/reguseq! treg_ :sec  #unit-map/useq[0 1 .. ##Inf])
  (sut/reguseq! treg_ :hour #unit-map/useq[0 1 .. ##Inf])
  (sut/reguseq! treg_ :day  #unit-map/useq[0 1 .. ##Inf]) #_"NOTE: should start with 0 or with 1?"

  (def timestamp  (sut/regusys! treg_ [:ms]))
  (def ms-hour    (sut/regusys! treg_ [:ms :sec :min :hour]))
  (def ms-day     (sut/regusys! treg_ [:ms :sec :min :hour :day]))
  (def ms-year    (sut/regusys! treg_ [:ms :sec :min :hour :day :month :year]))
  (def month-year (sut/regusys! treg_ [:month :year]))
  (def date       (sut/regusys! treg_ [:day :month :year])))


(t/deftest cmp
  (t/testing "eq?"
    (t/is (sut/eq? @treg_
                   {:day 26, :month :jul, :year 2020}))
    (t/is (sut/eq? @treg_
                   {:day 26, :month :jul, :year 2020} {:day 26, :month :jul, :year 2020} {:day 26, :month :jul, :year 2020}))
    (t/is (sut/eq? @treg_
                   {} {}))
    (t/is (sut/not-eq? @treg_ {} {:year 2020})))

  (t/testing "not-eq?"
    (t/is (not (sut/not-eq? @treg_ {:day 26, :month :jul, :year 2020})))
    (t/is (sut/not-eq? @treg_ {:day 25, :month :jul, :year 2020} {:day 26, :month :jul, :year 2020} {:day 26, :month :jul, :year 2020}))
    (t/is (sut/not-eq? @treg_ {} {:day 26, :month :jul, :year 2020})))

  (t/testing "lt?"
    (t/is (sut/lt? @treg_ {:day 26, :month :jul, :year 2020}))
    (t/is (sut/lt? @treg_ {:day 26, :month :jul, :year 2020} {:day 27, :month :jul, :year 2020} {:day 28, :month :jul, :year 2020}))
    (t/is (sut/lt? @treg_ {} {:day 26, :month :jul, :year 2020})))

  (t/testing "gt?"
    (t/is (sut/gt? @treg_ {:day 26, :month :jul, :year 2020}))
    (t/is (sut/gt? @treg_ {:day 27, :month :jul, :year 2020} {:day 26, :month :jul, :year 2020} {:day 25, :month :jul, :year 2020}))
    (t/is (sut/gt? @treg_ {:day 26, :month :jul, :year 2020} {})))

  (t/testing "lte?"
    (t/is (sut/lte? @treg_ {:day 26, :month :jul, :year 2020}))
    (t/is (sut/lte? @treg_ {:day 26, :month :jul, :year 2020} {:day 27, :month :jul, :year 2020} {:day 27, :month :jul, :year 2020}))
    (t/is (sut/lte? @treg_ {} {:day 26, :month :jul, :year 2020})))

  (t/testing "gte?"
    (t/is (sut/gte? @treg_ {:day 26, :month :jul, :year 2020}))
    (t/is (sut/gte? @treg_ {:day 27, :month :jul, :year 2020} {:day 26, :month :jul, :year 2020} {:day 26, :month :jul, :year 2020}))
    (t/is (sut/gte? @treg_ {:day 26, :month :jul, :year 2020} {}))))


(t/deftest arithmetic
  (t/testing "+"
    (def t
      {:year  2018
       :month :jan
       :day   1
       :hour  12
       :min   30
       :sec   30
       :ms    500})

    (t/is (= (merge t {:ms 700})
             (sut/add-delta @treg_
                            t {:ms 200})))

    (t/is (= (merge t {:ms 100, :sec 31})
             (sut/add-delta @treg_
                            t {:ms 600})))

    (t/is (= {:ms 1500}
             (sut/add-delta @treg_
                            {:ms 600} {:ms 600} {:ms 300})))

    (t/is (= {:ms 500, :sec 1}
             (sut/add-delta @treg_
                            {:sec 0, :ms 600} {:ms 600} {:ms 300})))

    (t/is (= (merge t {:sec 50})
             (sut/add-delta @treg_
                            t {:sec 20})))

    (t/is (= (merge t {:sec 50})
             (sut/add-delta @treg_
                            t {:sec 20})))

    (t/is (= (merge t {:hour 12, :min 50})
             (sut/add-delta @treg_
                            t {:min 20})))

    (t/is (= (merge t {:hour 13 :min 0})
             (sut/add-delta @treg_
                            t {:min 30})))

    (t/is (= {:year 2019 :month :jan :day 1}
             (sut/add-delta @treg_
                            {:year 2018 :month :dec :day 31} {:day 1})))

    (t/is (= {:year 2018 :month :feb :day 1}
             (sut/add-delta @treg_
                            {:year 2018 :month :jan :day 1} {:day 31})))

    (t/is (= {:year 2020 :month :jan :day 1}
             (sut/add-delta @treg_
                            {:year 2018 :month :dec :day 31} {:day 366})))

    (t/is (= {:year 2018 :month :mar :day 1}
             (sut/add-delta @treg_
                            {:year 2018 :month :feb :day 28} {:day 1})))

    (t/is (= {:year 2018 :month :mar :day 31}
             (sut/add-delta @treg_
                            {:year 2018 :month :mar :day 30} {:day 1})))

    (t/is (= {:year 2018 :month :apr :day 1}
             (sut/add-delta @treg_
                            {:year 2018 :month :mar :day 31} {:day 1})))

    (t/is (= {:ms 400}
             (sut/add-delta @treg_
                            {:ms 100} {:ms 300})))

    (t/is (= {:ms 200 :sec 1}
             (sut/add-delta @treg_
                            {:sec 0, :ms 900} {:ms 300})))

    (t/is (= {:sec 30 :min 1}
             (sut/add-delta @treg_
                            {:min 0, :sec 40} {:sec 50})))

    (t/is (= {:min 30 :hour 1}
             (sut/add-delta @treg_
                            {:min 40} {:min 50})))

    (t/is (= {:hour 3 :day 1}
             (sut/add-delta @treg_
                            {:day 0, :hour 13} {:hour 14})))

    (t/is (= {:year 2011 :month :jan :day 2 :hour 4}
             (sut/add-delta @treg_
                            {:year 2011 :month :jan :day 1 :hour 23} {:hour 5})))

    (t/is (= {:year 2011 :month :feb :day 2}
             (sut/add-delta @treg_
                            {:year 2011 :month :jan :day 30} {:day 3})))

    (t/is (= {:year 2012 :month :jan :day 1}
             (sut/add-delta @treg_
                            {:year 2011 :month :jan :day 1} {:day 365})))

    (t/is (= {:year 2012 :month :jan :day 1 :hour 4}
             (sut/add-delta @treg_
                            {:year 2011 :month :dec :day 31 :hour 23} {:hour 5})))

    (t/is (= {:year 2010 :month :dec :day 31 :hour 23}
             (sut/add-delta @treg_
                            {:year 2011 :month :jan :day 1 :hour 0} {:hour -1})))

    (t/is (= {:year 2010 :month :dec :day 31 :hour 23 :min 59 :sec 59}
             (sut/add-delta @treg_
                            {:year 2011 :month :jan :day 1 :hour 0} {:sec -1})))

    (t/is (= {:year 2010 :month :dec :day 31 :hour 23 :min 59 :sec 59 :ms 999}
             (sut/add-delta @treg_
                            {:year 2011 :month :jan :day 1 :hour 0} {:ms -1})))

    (t/is (= {:year 2010 :month :dec :day 31 :hour 23 :min 30}
             (sut/add-delta @treg_
                            {:year 2011 :month :jan :day 1 :hour 23} {:hour -23 :min -30})))

    (t/is (= {:year 2019 :month :dec :day 1}
             (sut/add-delta @treg_
                            {:year 2019 :month :nov :day 1} {:month 1})))

    (t/is (= {:year 2020 :month :jan :day 1}
             (sut/add-delta @treg_
                            {:year 2019 :month :nov :day 1} {:month 2})))

    (t/is (= {:year 2020 :month :jan :day 1}
             (sut/add-delta @treg_
                            {:year 2019 :month :dec :day 1} {:month 1})))

    (t/is (= {:year 2019 :month :dec :day 31}
             (sut/add-delta @treg_
                            {:year 2019 :month :nov :day 31} {:month 1})))

    (t/is (= {:year 2020 :month :feb}
             (sut/add-delta @treg_
                            {:year 2020 :month :feb} {:day 0})))

    (t/is (= {:year 2019, :month :dec, :day 10, :hour 15, :min 17, :sec 50, :ms 911}
             (sut/add-delta @treg_
                            {:year 2019, :month :dec, :day 10, :hour 13, :min 17, :sec 50, :ms 911} {:hour 2})))

    (t/is (= {:hour 14 :tz {:hour 2}}
             (sut/add-delta @treg_
                            {:hour 4 :tz {:hour 2}} {:hour 10})))

    (t/is (= {:hour 2 :tz {:hour -2}}
             (sut/add-delta @treg_
                            {:hour 1 :tz {:hour -2}} {:hour 1}))))

  (t/testing "-"
    (t/is (= {:year 2016, :month :jan, :day 1, :hour 23, :min 30}
             (sut/subtract-delta @treg_
                                 {:year 2016, :month :dec, :day 31, :hour 23, :min 30} {:day 365})))

    (t/is (= {:year 2015, :month :dec, :day 31, :hour 23, :min 30}
             (sut/subtract-delta @treg_
                                 {:year 2016 :month :dec :day 31 :hour 23 :min 30} {:day 366})))

    (t/is (= {:year 2020 :month :jan :day 31}
             (sut/subtract-delta @treg_
                                 {:year 2020 :month :feb}
                                 {:day 1})))
    (t/is (= {:year 2020 :month :feb}
             (sut/subtract-delta @treg_
                                 {:year 2020 :month :feb}
                                 {:day 0})))

    (t/is (sut/eq? @treg_
                   {:hour 0, :tz {:hour -2}}
                   (sut/subtract-delta @treg_
                                       {:hour 2 :tz {:hour -2}} {:hour 2})))
    (t/is (sut/eq? @treg_
                   {:hour 0}
                   (sut/subtract-delta @treg_
                                       {:hour 2 :tz {:hour -2}} {:hour 2})))
    (t/is (sut/eq? @treg_
                   {:hour 2}
                   (sut/subtract-delta @treg_
                                       {:hour 3 :tz {:hour 2}} {:hour 1 :tz {:hour 2}}))))

  (t/testing "difference"
    (t/is (= {:day 6}
             (sut/difference @treg_
                             {:day 20, :month :jul, :year 2020}
                             {:day 26, :month :jul, :year 2020})))
    (t/is (= {:day 6}
             (sut/difference @treg_
                             {:day 26, :month :jul, :year 2020}
                             {:day 20, :month :jul, :year 2020})))
    (t/is (= {:day 22, :year 23}
             (sut/difference @treg_
                             {:day 27, :month :jul, :year 2020}
                             {:day 5, :month :jul, :year 1997})))
    (t/is (= {:day 22, :year 23}
             (sut/difference @treg_
                             {:day 5, :month :jul, :year 1997}
                             {:day 27, :month :jul, :year 2020})))
    (t/is (= {:year 1}
             (sut/difference @treg_
                             {:year 1} {:year -1})))
    (t/is (= {:year 1}
             (sut/difference @treg_
                             {:year -1} {:year 1})))
    (t/is (empty? (sut/difference @treg_
                                  {:day 27, :month :jul, :year 2020}
                                  {:day 27, :month :jul, :year 2020})))
    (t/is (= {:day 1}
             (sut/difference @treg_
                             {:day 28, :month :jul, :year 2020}
                             {:day 27, :month :jul, :year 2020})))
    #_(t/is (= {:hour 0}
               (sut/difference @treg_
                               {:hour 12, :min 30, :tz {:hour -2}}
                               {:hour 14, :min 30, :tz {:hour 0}})))
    (t/is (= {:year 2, :day 5}
             (sut/difference @treg_
                             {:day 28, :month :jun, :year 2020}
                             {:day 3, :month :jul, :year 2022})))
    (t/is (= {:year 2, :month 1, :day 6}
             (sut/difference @treg_
                             {:day 28, :month :may, :year 2020}
                             {:day 3, :month :jul, :year 2022})))
    (t/is (= {:day 5}
             (sut/difference @treg_
                             {:day 28, :month :jun, :year 2022}
                             {:day 3, :month :jul, :year 2022})))
    (t/is (= {:day 2}
             (sut/difference @treg_
                             {:day 1, :month :mar, :year 2020}
                             {:day 28, :month :feb, :year 2020})))
    (t/is (= {:day 1}
             (sut/difference @treg_
                             {:day 1, :month :mar, :year 2021}
                             {:day 28, :month :feb, :year 2021})))
    (t/is (= {:year 2, :day 2}
             (sut/difference @treg_
                             {:day 1, :month :mar, :year 2022}
                             {:day 28, :month :feb, :year 2020})))
    (t/is (= {:year 1, :day 1}
             (sut/difference @treg_
                             {:day 1, :month :mar, :year 2020}
                             {:day 28, :month :feb, :year 2019})))
    (t/is (= {:month 11, :day 27}
             (sut/difference @treg_
                             {:day 1, :month :mar, :year 2020}
                             {:day 28, :month :feb, :year 2021})))
    (t/is (= {:month 11, :day 1}
             (sut/difference @treg_
                             {:day 1, :month :mar, :year 2021}
                             {:day 31, :month :mar, :year 2020})))


    (t/is (= {:month 1, :day 3}
             (sut/difference @treg_
                             {:day 29, :month :jan, :year 2022}
                             {:day 1, :month :mar, :year 2022})))
    (t/is (= {:month 1, :day 2}
             (sut/difference @treg_
                             {:day 30, :month :jan, :year 2022}
                             {:day 1, :month :mar, :year 2022})))
    (t/is (= {:month 1, :day 1}
             (sut/difference @treg_
                             {:day 31, :month :jan, :year 2022}
                             {:day 1, :month :mar, :year 2022}))))

  #_(t/testing "difference-in"
      (t/is (= {:day 1010}
               (sut/difference-in [:ms :day]
                                  {:year 2019, :month :jul, :day 28}
                                  {:year 2022, :month :may, :day 3})))

      (t/is (= {:year 2, :month 9}
               (sut/difference-in [:month :year]
                                  {:year 2019, :month :jul, :day 28}
                                  {:year 2022, :month :may, :day 3})))

      (t/is (= {:month 33}
               (sut/difference-in [:month]
                                  {:year 2019, :month :jul, :day 28}
                                  {:year 2022, :month :may, :day 3})))

      (t/is (= {:year 2, :month 9, :day 6}
               (sut/difference-in [:month :day :year]
                                  {:year 2019, :month :jul, :day 28}
                                  {:year 2022, :month :may, :day 3})))

      (t/is (= {:day 1009, :ms 48600000}
               (sut/difference-in [:ms :day]
                                  {:year 2019, :month :jul, :day 28
                                   :hour 10, :min 30}
                                  {:year 2022, :month :may, :day 3})))))


(t/deftest ^:kaocha/pending demo-test
  (sut/reguseq! treg_ :ms   #unit-map/useq[0 1 .. 999] :next :sec)
  (sut/reguseq! treg_ :sec  #unit-map/useq[0 1 .. 59] :next :min)
  (sut/reguseq! treg_ :min  #unit-map/useq[0 1 .. 59] :next :hour)
  (sut/reguseq! treg_ :hour #unit-map/useq[0 1 .. 23] :next :day)

  (sut/reguseq! treg_ :day   #unit-map/useq[1 2 .. days-in-month] :next :month)
  (sut/reguseq! treg_ :month #unit-map/useq[:jan :feb  :mar :apr :may  :jun :jul :aug  :sep :oct :nov  :dec] :next :year)
  (sut/reguseq! treg_ :year  #unit-map/useq[##-Inf .. -2 -1 1 2 .. ##Inf])

  (sut/regusys! treg_ [:ms :sec :min :hour :day :month :year])

  #_(sut/deffmt :iso/month [:month (fn [v fmt-el] '???)])
  #_(sut/deffmt :iso/day [:day 2 "0"])

  #_"NOTE: arithmetics for now can be stubbed with simple update/inc etc"
  #_"NOTE: need some configs to map months enum to numbers"
  #_"NOTE: for useqs consisting of only static uranges calculate leading 0 padding automatically"

  (defn job-status-at [job {:keys [current-time in-fmt out-fmt]}]
    #_"TODO")

  (t/is (= (job-status-at
             {:resourceType "Job"
              :name         "denormalize"
              :start-at     {:hour 5}
              :last-run     "2022-04-01T05:00:00.000"}
             {:current-time "2022-04-01T14:30:00.000"
              :in-fmt  [:year \- :iso/month \- :iso/day \T :hour \: :min \: :sec \. :ms]
              :out-fmt [:year \- :iso/month \- :iso/day \T :hour \: :min \: :sec \. :ms]})
           {:latst-run           "2022-04-01T05:00:00.000"
            :next-run            "2022-04-02T05:00:00.000"
            :should-start-now?   false
            :time-until-next-run {:hour 14, :min 30}})))
