(ns chrono.new-ops-test
  (:require [chrono.new-ops :as sut]
            [chrono.type.datetime :as datetime]
            [chrono.type.time :as time]
            [chrono.type.date :as date]
            [matcho.core :as matcho]
            [clojure.test :as t]))

(defmethod sut/type :default-type [_] datetime/gregorian-military)

(t/deftest range-test
  (def base60   (:min  (sut/type {})))
  (def days     (:day (sut/type {})))
  (def months   (:month (sut/type {})))
  (def years    (:year (sut/type {})))
  (def am-hours (:hour (sut/type ^::time/am-pm{})))

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
                            ^:delta{:day 99, :month -99, :year 0, :sec 30, :foo 1})
                  {:sec 30, :day 10, :month :mar :year 2011, :foo 1})
    (matcho/match (sut/plus {:day 1, :month :mar, :year 2019}
                            ^:delta{:year 0}
                            ^:delta{:month -99}
                            ^:delta{:day 99}
                            ^:delta{:sec 30}
                            ^:delta{:foo 1})
                  {:sec 30, :day 10, :month :mar :year 2011, :foo 1}))

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
                  {:day 22, :year 23})
    (matcho/match (sut/difference {:day 5, :month :jul, :year 1997}
                                  {:day 27, :month :jul, :year 2020})
                  {:day 22, :year 23}))

  (t/testing "minus"
    (matcho/match (sut/minus ^:delta{:day 1, :month 3})
                  ^:delta{:day -1, :month -3})
    (matcho/match (sut/minus {:day 1, :month :mar, :year 2019}
                             ^:delta{:day -99, :month 99, :year 0, :sec -30, :foo -1})
                  {:sec 30, :day 10, :month :mar :year 2011, :foo 1})
    (matcho/match (sut/minus {:day 1, :month :mar, :year 2019}
                             ^:delta{:year 0}
                             ^:delta{:month 99}
                             ^:delta{:day -99}
                             ^:delta{:sec -30}
                             ^:delta{:foo -1})
                  {:sec 30, :day 10, :month :mar :year 2011, :foo 1})

    (matcho/match (sut/minus {:day 20, :month :jul, :year 2020}
                             {:day 26, :month :jul, :year 2020})
                  ^:delta{:day -6})
    (matcho/match (sut/minus {:day 26, :month :jul, :year 2020}
                             {:day 20, :month :jul, :year 2020})
                  ^:delta{:day 6})
    (matcho/match (sut/minus {:day 27, :month :jul, :year 2020}
                             {:day 5, :month :jul, :year 1997})
                  {:day 22, :year 23})
    (matcho/match (sut/minus {:day 5, :month :jul, :year 1997}
                             {:day 27, :month :jul, :year 2020})
                  {:day -22, :year -23}))

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
    (t/is (sut/gte? {:day 26, :month :jul, :year 2020} {}))))
