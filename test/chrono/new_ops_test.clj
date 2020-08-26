(ns chrono.new-ops-test
  (:require [chrono.new-ops :as sut]
            [chrono.type.datetime :as datetime]
            [chrono.type.time :as time]
            [chrono.type.date :as date]
            [matcho.core :as matcho]
            [clojure.test :as t]))

(defmethod sut/definition :default-type [_] datetime/gregorian-military)

(t/deftest range-test
  (def base60   (:min  (sut/definition {})))
  (def days     (:day (sut/definition {})))
  (def months   (:month (sut/definition {})))
  (def years    (:year (sut/definition {})))
  (def am-hours (:hour (sut/definition ^::time/am-pm{})))

  (t/testing "type-test"
    (t/is (= [::time/military]        (sut/get-type ^::time/military{:hour 10})))
    (t/is (= [::time/military :delta] (sut/get-type ^{::time/military :delta}{:hour 1})))
    (t/is (= [::time/military :tz]    (sut/get-type ^{::time/military :tz}{:hour 1})))

    (t/is (= [:default-type]        (sut/get-type {:hour 10})))
    (t/is (= [:default-type :delta] (sut/get-type ^:delta{:hour 1})))
    (t/is (= [:default-type :tz]    (sut/get-type ^:tz{:hour 1})))

    (t/is (= [::time/military :delta] (sut/get-type (sut/value->delta ^::time/military{:hour 1}))))
    (t/is (= [:default-type :delta]   (sut/get-type (sut/value->delta {:hour 1}))))

    (matcho/match (sut/definition ^::time/military{:hour 20})
                  '{:ms   [0 1 .. 999]
                    :sec  [0 1 .. 59]
                    :min  [0 1 .. 59]
                    :hour [0 1 .. 23]})

    (matcho/match (sut/definition ^{::time/military :tz}{:hour 20})
                  '{:ms   [0 1 .. 999]
                    :sec  [0 1 .. 59]
                    :min  [0 1 .. 59]
                    :hour [0 1 .. 23]})

    (matcho/match (sut/definition ^::datetime/military{:hour 20, :year 2020, :day 26, :month 8})
                  {:ms    [0 1 '.. 999]
                   :sec   [0 1 '.. 59]
                   :min   [0 1 '.. 59]
                   :hour  [0 1 '.. 23]
                   :day   [1 2 '.. fn?]
                   :month [:jan :feb :mar :apr :may :jun :jul :aug :sep :oct :nov :dec]
                   :year  [##-Inf '.. -2 -1 1 2 '.. ##Inf]})

    (matcho/match (sut/definition ^{::datetime/military :tz}{:hour 2, :month 12, :year 2020})
                  {:ms    [0 1 '.. 999]
                   :sec   [0 1 '.. 59]
                   :min   [0 1 '.. 59]
                   :hour  [0 1 '.. 23]
                   :day   [0 1 '.. 30]
                   :month [0 1 '.. 11]
                   :year  [##-Inf '.. -2 -1 0 1 '.. ##Inf]}))

  (t/testing "process-sequence"
    (matcho/match (sut/process-sequence base60)
                  [{:start 0, :step 1, :end 59}])

    (matcho/match (sut/process-sequence months)
                  [:jan :feb :mar :apr :may :jun :jul :aug :sep :oct :nov :dec])

    (matcho/match (sut/process-sequence years)
                  [{:start ##-Inf, :step 1, :end -1}
                   {:start 1, :step 1, :end ##Inf}])

    (matcho/match (sut/process-sequence am-hours)
                  [12 {:start 1, :step 1, :end 11}])

    (matcho/match (sut/process-sequence days)
                  [{:start 1, :step 1, :end fn?}])

    (matcho/match (sut/process-sequence [1 3 '.. :TODO-REMOVE (fn [{:keys [bar]}] (if (odd? bar) 9 11)) 13 15])
                  [{:start 1, :step 2, :end fn?} 13 15]))

  (t/testing "sequence-length"
    (t/is (= 12    (sut/sequence-length [:jan :feb :mar :apr :may :jun :jul :aug :sep :oct :nov :dec] {})))
    (t/is (= ##Inf (sut/sequence-length [##-Inf '.. -2 -1 1 2 '.. ##Inf] {})))
    (t/is (= 1000  (sut/sequence-length [0 1 '.. 999] {})))

    (t/is (= 0      (sut/sequence-first-index [:jan :feb :mar :apr :may :jun :jul :aug :sep :oct :nov :dec] {})))
    (t/is (= ##-Inf (sut/sequence-first-index [##-Inf '.. -2 -1 1 2 '.. ##Inf] {})))
    (t/is (= 0      (sut/sequence-first-index [0 1 '.. 999] {})))

    (t/is (= 11    (sut/sequence-last-index [:jan :feb :mar :apr :may :jun :jul :aug :sep :oct :nov :dec] {})))
    (t/is (= ##Inf (sut/sequence-last-index [##-Inf '.. -2 -1 1 2 '.. ##Inf] {})))
    (t/is (= 999   (sut/sequence-last-index [0 1 '.. 999] {}))))

  (t/testing "sequence-contains-some"
    (matcho/match (map (comp boolean (partial sut/sequence-contains-some base60 nil))
                       [##-Inf -1 0 59 60 ##Inf])
                  [false false true true false false])
    (matcho/match (map (comp boolean (partial sut/sequence-contains-some years nil))
                       [##-Inf -31337 -1 0 1 31337 ##Inf])
                  [true true true false true true true])
    (matcho/match (map (comp boolean (partial sut/sequence-contains-some days {:month :feb, :year 2020}))
                       [0 1 28 29 30])
                  [false true true true false])
    (matcho/match (map (comp boolean (partial sut/sequence-contains-some days {:month :feb, :year 2019}))
                       [0 1 28 29 30])
                  [false true true false false]))

  (t/testing "get-next-unit-value"
    (matcho/match (take-while some? (iterate (partial sut/get-next-unit-value base60 nil) 0))
                  (range 60))

    (matcho/match (take-while some? (iterate (partial sut/get-next-unit-value months nil) :jan))
                  [:jan :feb :mar :apr :may :jun :jul :aug :sep :oct :nov :dec])

    (matcho/match (take 51 (iterate (partial sut/get-next-unit-value years nil) 1970))
                  (range 1970 2021))

    (matcho/match (take-while some? (iterate (partial sut/get-next-unit-value am-hours nil) 12))
                  [12 1 2 3 4 5 6 7 8 9 10 11])
    (t/is (= 13 (sut/get-next-unit-value [1 3 '.. :TODO-REMOVE (fn [{:keys [bar]}] (if (odd? bar) 9 11)) 13 15]
                                         {:bar 7}
                                         9)))
    (t/is (= 11 (sut/get-next-unit-value [1 3 '.. :TODO-REMOVE (fn [{:keys [bar]}] (if (odd? bar) 9 11)) 13 15]
                                         {:bar 8}
                                         9))))

  (t/testing "get-prev-unit-value"
    (matcho/match (take-while some? (iterate (partial sut/get-prev-unit-value base60 nil) 59))
                  (range 59 -1 -1))

    (matcho/match (take-while some? (iterate (partial sut/get-prev-unit-value months nil) :dec))
                  [:dec :nov :oct :sep :aug :jul :jun :may :apr :mar :feb :jan])

    (matcho/match (take 51 (iterate (partial sut/get-prev-unit-value years nil) 1970))
                  (range 1970 1920))

    (matcho/match (take-while some? (iterate (partial sut/get-prev-unit-value am-hours nil) 11))
                  [11 10 9 8 7 6 5 4 3 2 1 12]))

  (t/testing "inc-unit"
    (let [value ^::time/am-pm{:hour 12, :period :am}]
      (matcho/match (take 24 (iterate (partial sut/inc-unit :hour) value))
                    [{:hour 12, :period :am} {:hour 1, :period :am} {:hour 2, :period :am} {:hour 3, :period :am} {:hour 4, :period :am} {:hour 5, :period :am} {:hour 6, :period :am} {:hour 7, :period :am} {:hour 8, :period :am} {:hour 9, :period :am} {:hour 10, :period :am} {:hour 11, :period :am}
                     {:hour 12, :period :pm} {:hour 1, :period :pm} {:hour 2, :period :pm} {:hour 3, :period :pm} {:hour 4, :period :pm} {:hour 5, :period :pm} {:hour 6, :period :pm} {:hour 7, :period :pm} {:hour 8, :period :pm} {:hour 9, :period :pm} {:hour 10, :period :pm} {:hour 11, :period :pm}]))

    (let [value ^::date/gregorian{:day 1, :month :jan, :year 2020}
          calendar (->> value
                        (iterate (partial sut/inc-unit :day))
                        (take-while (comp #{2020} :year))
                        (partition-by :month))]
      (t/is (= 12 (count calendar)))
      (t/is (= 366 (count (flatten calendar))))))


  (t/testing "dec-unit"
    (let [value ^::time/am-pm{:hour 11, :period :pm}]
      (matcho/match (take 24 (iterate (partial sut/dec-unit :hour) value))
                    [{:hour 11, :period :pm} {:hour 10, :period :pm} {:hour 9, :period :pm} {:hour 8, :period :pm} {:hour 7, :period :pm} {:hour 6, :period :pm} {:hour 5, :period :pm} {:hour 4, :period :pm} {:hour 3, :period :pm} {:hour 2, :period :pm} {:hour 1, :period :pm} {:hour 12, :period :pm}
                     {:hour 11, :period :am} {:hour 10, :period :am} {:hour 9, :period :am} {:hour 8, :period :am} {:hour 7, :period :am} {:hour 6, :period :am} {:hour 5, :period :am} {:hour 4, :period :am} {:hour 3, :period :am} {:hour 2, :period :am} {:hour 1, :period :am} {:hour 12, :period :am}]))

    (let [value ^::date/gregorian{:day 31, :month :dec, :year 2019}
          calendar (->> value
                        (iterate (partial sut/dec-unit :day))
                        (take-while (comp #{2019} :year))
                        (partition-by :month))]
      (t/is (= 12 (count calendar)))
      (t/is (= 365 (count (flatten calendar))))
      calendar))

  (t/testing "add-to-unit"
    (matcho/match (sut/add-to-unit :day {:day 1, :month :jan, :year 2020} 9000)
                  {:day 22, :month :aug, :year 2044})
    (matcho/match (sut/add-to-unit :day {:day 1, :month :jan, :year 2020} -9000)
                  {:year 1995, :month :may, :day 12})
    (matcho/match (sut/add-to-unit :day {:day 1, :month :jan, :year 2020} 0)
                  {:day 1, :month :jan, :year 2020}))

  (t/testing "substract-from-unit"
    (matcho/match (sut/substract-from-unit :day {:day 1, :month :jan, :year 2020} 9000)
                  {:year 1995, :month :may, :day 12})
    (matcho/match (sut/substract-from-unit :day {:day 1, :month :jan, :year 2020} -9000)
                  {:day 22, :month :aug, :year 2044})
    (matcho/match (sut/substract-from-unit :day {:day 1, :month :jan, :year 2020} 0)
                  {:day 1, :month :jan, :year 2020}))

  (t/testing "plus"
    (matcho/match (sut/plus {:day 1, :month :mar, :year 2019})
                  {:day 1, :month :mar, :year 2019})
    (matcho/match (sut/plus {:day 1, :month :mar, :year 2019}
                            ^:delta{})
                  {:day 1, :month :mar, :year 2019})
    (matcho/match (sut/plus {:day 1, :month :mar, :year 2019}
                            ^:delta{:year 1, :day 1})
                  {:day 2, :year 2020})
    (matcho/match (sut/plus {:day 1, :month :mar, :year 2019}
                            ^:delta{:day 99, :month -99, :year 0, :sec 30})
                  {:sec 30, :day 10, :month :mar :year 2011})
    (matcho/match (sut/plus {:day 1, :month :mar, :year 2019}
                            ^:delta{:year 0}
                            ^:delta{:month -99}
                            ^:delta{:day 99}
                            ^:delta{:sec 30})
                  {:sec 30, :day 10, :month :mar :year 2011}))

  (t/testing "invert"
    (matcho/match (sut/invert ^:delta{:day 1, :month 3})
                  ^:delta{:day -1, :month -3}))

  (t/testing "difference"
    (matcho/match (sut/difference {:day 20, :month :jul, :year 2020}
                                  {:day 26, :month :jul, :year 2020})
                  ^:delta{:day 6})
    (matcho/match (sut/difference {:day 26, :month :jul, :year 2020}
                                  {:day 20, :month :jul, :year 2020})
                  ^:delta{:day 6})
    (matcho/match (sut/difference {:day 27, :month :jul, :year 2020}
                                  {:day 5, :month :jul, :year 1997})
                  ^:delta{:day 22, :year 23})
    (matcho/match (sut/difference {:day 5, :month :jul, :year 1997}
                                  {:day 27, :month :jul, :year 2020})
                  ^:delta{:day 22, :year 23})
    (matcho/match (sut/difference {:year 1} {:year -1})
                  ^:delta{:year 1})
    (matcho/match (sut/difference {:year -1} {:year 1})
                  ^:delta{:year 1})
    (t/is (= ^:delta{}
             (sut/difference {:day 27, :month :jul, :year 2020}
                             {:day 27, :month :jul, :year 2020})))
    (t/is (= ^:delta{:day 1}
             (sut/difference {:day 28, :month :jul, :year 2020}
                             {:day 27, :month :jul, :year 2020}))))

  (t/testing "minus"
    (matcho/match (sut/minus ^:delta{:day 1, :month 3})
                  ^:delta{:day -1, :month -3})
    (matcho/match (sut/minus {:day 1, :month :mar, :year 2019}
                             ^:delta{:day -99, :month 99, :year 0, :sec -30})
                  {:sec 30, :day 10, :month :mar :year 2011})
    (matcho/match (sut/minus {:day 1, :month :mar, :year 2019}
                             ^:delta{:year 0}
                             ^:delta{:month 99}
                             ^:delta{:day -99}
                             ^:delta{:sec -30})
                  {:sec 30, :day 10, :month :mar :year 2011})

    (matcho/match (sut/minus {:day 20, :month :jul, :year 2020}
                             {:day 26, :month :jul, :year 2020})
                  ^:delta{:day -6})
    (matcho/match (sut/minus {:day 26, :month :jul, :year 2020}
                             {:day 20, :month :jul, :year 2020})
                  ^:delta{:day 6})
    (matcho/match (sut/minus {:day 27, :month :jul, :year 2020}
                             {:day 5, :month :jul, :year 1997})
                  ^:delta{:day 22, :year 23})
    (matcho/match (sut/minus {:day 5, :month :jul, :year 1997}
                             {:day 27, :month :jul, :year 2020})
                  ^:delta{:day -22, :year -23})
    (matcho/match (sut/minus {:year -1}
                             {:year 1})
                  ^:delta{:year -1})
    (matcho/match (sut/minus {:year 1}
                             {:year -1})
                  ^:delta{:year 1})
    (t/is (= (sut/minus {:day 27, :month :jul, :year 2020}
                        {:day 26, :month :jul, :year 2020})
             ^:delta{:day 1})))

  (t/testing "apply-delta"
    (matcho/match (sut/apply-delta {:year 2020, :month 7, :day 29, :hour 17, :min 20, :sec 50, :ms 733}
                                   ^:tz{:hour 2})
                  {:year 2020, :month 7, :day 29, :hour 19, :min 20, :sec 50, :ms 733, :tz {:hour 2}})
    (matcho/match (sut/get-applied-deltas
                   (sut/apply-delta {:year 2020, :month 7, :day 29, :hour 17, :min 20, :sec 50, :ms 733}
                                    ^:tz{:hour 2}))
                  [{:hour 2}]))

  (t/testing "eq?"
    (t/is (sut/eq? {:day 26, :month :jul, :year 2020}))
    (t/is (sut/eq? {:day 26, :month :jul, :year 2020} {:day 26, :month :jul, :year 2020} {:day 26, :month :jul, :year 2020}))
    (t/is (sut/eq? {} {}))
    (t/is (sut/not-eq? {} {:year 2020})))

  (t/testing "not-eq?"
    (t/is (not (sut/not-eq? {:day 26, :month :jul, :year 2020})))
    (t/is (sut/not-eq? {:day 25, :month :jul, :year 2020} {:day 26, :month :jul, :year 2020} {:day 26, :month :jul, :year 2020}))
    (t/is (sut/not-eq? {} {:day 26, :month :jul, :year 2020})))

  (t/testing "lt?"
    (t/is (sut/lt? {:day 26, :month :jul, :year 2020}))
    (t/is (sut/lt? {:day 26, :month :jul, :year 2020} {:day 27, :month :jul, :year 2020} {:day 28, :month :jul, :year 2020}))
    (t/is (sut/lt? {} {:day 26, :month :jul, :year 2020})))

  (t/testing "gt?"
    (t/is (sut/gt? {:day 26, :month :jul, :year 2020}))
    (t/is (sut/gt? {:day 27, :month :jul, :year 2020} {:day 26, :month :jul, :year 2020} {:day 25, :month :jul, :year 2020}))
    (t/is (sut/gt? {:day 26, :month :jul, :year 2020} {})))

  (t/testing "lte?"
    (t/is (sut/lte? {:day 26, :month :jul, :year 2020}))
    (t/is (sut/lte? {:day 26, :month :jul, :year 2020} {:day 27, :month :jul, :year 2020} {:day 27, :month :jul, :year 2020}))
    (t/is (sut/lte? {} {:day 26, :month :jul, :year 2020})))

  (t/testing "gte?"
    (t/is (sut/gte? {:day 26, :month :jul, :year 2020}))
    (t/is (sut/gte? {:day 27, :month :jul, :year 2020} {:day 26, :month :jul, :year 2020} {:day 26, :month :jul, :year 2020}))
    (t/is (sut/gte? {:day 26, :month :jul, :year 2020} {})))

  (t/testing "cmp delta"
    (t/is (= 0 (sut/cmp ^{::time/military :tz}{:hour 20}
                        ^{::time/military :tz}{:hour 20, :min 0})))))
