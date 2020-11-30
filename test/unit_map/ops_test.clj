(ns unit-map.ops-test
  (:require [unit-map.ops :as sut]
            [unit-map.type.chrono.datetime :as datetime]
            [unit-map.type.chrono.time :as time]
            [unit-map.type.chrono.date :as date]
            [matcho.core :as matcho]
            [clojure.test :as t]))


;; TODO: use sut/eq? instead of matcho/match & t/is


(t/use-fixtures
  :each
  (fn [t]
    (defmethod sut/definition :type/default-type [_] datetime/gregorian-military)
    (t)))


(t/deftest type-exception-test
  (remove-method sut/definition :type/default-type)
  (def exception nil)
  (try (sut/definition {:hour 1})
       (catch clojure.lang.ExceptionInfo e
         (def exception (bean e)))
       (finally (matcho/match exception
                              (-> (sut/no-default-type-exception {:hour 1})
                                  bean
                                  (select-keys [:class :data])))))
  (try (sut/definition ^:delta{:hour 1})
       (catch clojure.lang.ExceptionInfo e
         (def exception (bean e)))
       (finally (matcho/match exception
                              (-> (sut/no-default-type-exception ^:delta{:hour 1})
                                  bean
                                  (select-keys [:class :data])))))
  (try (sut/get-type ^:date{})
       (catch clojure.lang.ExceptionInfo e
         (def exception (bean e)))
       (finally (matcho/match exception
                              (-> (sut/no-default-type-exception ^:date{})
                                  bean
                                  (select-keys [:class :data])))))

  (t/is (= [::datetime/military]
           (sut/get-type ^::datetime/military{}))))


(t/deftest ops-test
  (def base60   (sut/unit-definition {} :min))
  (def days     (sut/unit-definition {} :day))
  (def months   (sut/unit-definition {} :month))
  (def years    (sut/unit-definition {} :year))
  (def am-hours (sut/unit-definition ^::time/am-pm{} :hour))

  (t/testing "type-test"
    (t/is (= [::time/military]        (sut/get-type ^::time/military{:hour 10})))
    (t/is (= [::time/military :delta] (sut/get-type ^{::time/military :delta}{:hour 1})))
    (t/is (= [::time/military :tz]    (sut/get-type ^{::time/military :tz}{:hour 1})))

    (t/is (= [:type/default-type]        (sut/get-type {:hour 10})))
    (t/is (= [:type/default-type :delta] (sut/get-type ^:delta{:hour 1})))
    (t/is (= [:type/default-type :tz]    (sut/get-type ^:tz{:hour 1})))

    (t/is (= [::time/military :delta] (sut/get-type (sut/value->delta ^::time/military{:hour 1}))))
    (t/is (= [:type/default-type :delta]   (sut/get-type (sut/value->delta {:hour 1}))))

    (matcho/match (sut/definition ^::time/military{:hour 20})
                  #unit-map/definition[:ms   [0 1 .. 999]
                                     :sec  [0 1 .. 59]
                                     :min  [0 1 .. 59]
                                     :hour [0 1 .. 23]])

    (matcho/match (sut/definition ^{::time/military :tz}{:hour 20})
                  #unit-map/definition[:ms   [0 1 .. 999]
                                     :sec  [0 1 .. 59]
                                     :min  [0 1 .. 59]
                                     :hour [0 1 .. 23]])

    (matcho/match (sut/definition ^::datetime/military{:hour 20, :year 2020, :day 26, :month 8})
                  #unit-map/definition[:ms    [0 1 .. 999]
                                     :sec   [0 1 .. 59]
                                     :min   [0 1 .. 59]
                                     :hour  [0 1 .. 23]
                                     :day   [1 2 .. fn?]
                                     :month [1 2 .. 12]
                                     :year  [##-Inf .. -2 -1 1 2 .. ##Inf]])

    (matcho/match (sut/definition ^{::datetime/military :tz}{:hour 2, :month 12, :year 2020})
                  #unit-map/definition[:ms    [0 1 .. 999]
                                     :sec   [0 1 .. 59]
                                     :min   [0 1 .. 59]
                                     :hour  [0 1 .. 23]
                                     :day   [0 1 .. 30]
                                     :month [0 1 .. 11]
                                     :year  [##-Inf .. -2 -1 0 1 .. ##Inf]]))

  (t/testing "process-sequence"
    (matcho/match (sut/process-sequence base60)
                  [{:start 0, :step 1, :end 59}])

    (matcho/match (sut/process-sequence months)
                  [{:start 1, :step 1, :end 12}])

    (matcho/match (sut/process-sequence years)
                  [{:start ##-Inf, :step 1, :end -1}
                   {:start 1, :step 1, :end ##Inf}])

    (matcho/match (sut/process-sequence am-hours)
                  [12 {:start 1, :step 1, :end 11}])

    (matcho/match (sut/process-sequence days)
                  [{:start 1, :step 1, :end fn?}])

    (matcho/match #unit-map/sequence[1 3 .. :TODO-REMOVE (fn [{:keys [bar]}] (if (odd? bar) 9 11)) 13 15]
                  [{:start 1, :step 2, :end fn?} 13 15]))

  (t/testing "sequence-length"
    (t/is (= 12    (sut/sequence-length #unit-map/sequence[1 2 .. 12] {})))
    (t/is (= ##Inf (sut/sequence-length #unit-map/sequence[##-Inf .. -2 -1 1 2 .. ##Inf] {})))
    (t/is (= 1000  (sut/sequence-length #unit-map/sequence[0 1 .. 999] {})))

    (t/is (= 0      (sut/sequence-first-index #unit-map/sequence[1 2 .. 12] {})))
    (t/is (= ##-Inf (sut/sequence-first-index #unit-map/sequence[##-Inf .. -2 -1 1 2 .. ##Inf] {})))
    (t/is (= 0      (sut/sequence-first-index #unit-map/sequence[0 1 .. 999] {})))
    (t/is (= nil    (sut/sequence-first-index (sut/process-sequence []) {})))

    (t/is (= 11    (sut/sequence-last-index #unit-map/sequence[1 2 .. 12] {})))
    (t/is (= ##Inf (sut/sequence-last-index #unit-map/sequence[##-Inf .. -2 -1 1 2 .. ##Inf] {})))
    (t/is (= 999   (sut/sequence-last-index #unit-map/sequence[0 1 .. 999] {})))
    (t/is (= nil   (sut/sequence-last-index #unit-map/sequence[] {})))

    (for [x [##-Inf -100 -3 -2 -1 1 2 3 100 ##Inf]]
      (t/is (->> x
                 (sut/index-in-sequence years {})
                 (sut/sequence-nth years {})
                 (= x))))

    (t/is (nil? (sut/index-in-sequence years {} 0))))

  (t/testing "sequence-contains-some"
    (matcho/match (map (comp boolean (partial sut/sequence-contains-some base60 nil))
                       [##-Inf -1 0 59 60 ##Inf])
                  [false false true true false false])
    (matcho/match (map (comp boolean (partial sut/sequence-contains-some years nil))
                       [##-Inf -31337 -1 0 1 31337 ##Inf])
                  [true true true false true true true])
    (matcho/match (map (comp boolean (partial sut/sequence-contains-some days {:month 2, :year 2020}))
                       [0 1 28 29 30])
                  [false true true true false])
    (matcho/match (map (comp boolean (partial sut/sequence-contains-some days {:month 2, :year 2019}))
                       [0 1 28 29 30])
                  [false true true false false]))

  (t/testing "get-next-unit-value"
    (matcho/match (take-while some? (iterate (partial sut/get-next-unit-value base60 nil) 0))
                  (range 60))

    (matcho/match (take-while some? (iterate (partial sut/get-next-unit-value months nil) 1))
                  [1 2 3 4 5 6 7 8 9 10 11 12])

    (matcho/match (take 51 (iterate (partial sut/get-next-unit-value years nil) 1970))
                  (range 1970 2021))

    (matcho/match (take-while some? (iterate (partial sut/get-next-unit-value am-hours nil) 12))
                  [12 1 2 3 4 5 6 7 8 9 10 11])
    (t/is (= 13 (sut/get-next-unit-value #unit-map/sequence[1 3 .. :TODO-REMOVE (fn [{:keys [bar]}] (if (odd? bar) 9 11)) 13 15]
                                         {:bar 7}
                                         9)))
    (t/is (= 11 (sut/get-next-unit-value #unit-map/sequence[1 3 .. :TODO-REMOVE (fn [{:keys [bar]}] (if (odd? bar) 9 11)) 13 15]
                                         {:bar 8}
                                         9))))

  (t/testing "sequence->vector"
    (t/is (= [11 10 9 8 7 6 5 4 3 2]
             (vec (sut/sequence->vector #unit-map/sequence[11 10 .. 2] nil)))))

  (t/testing "get-prev-unit-value"
    (matcho/match (take-while some? (iterate (partial sut/get-prev-unit-value base60 nil) 59))
                  (range 59 -1 -1))

    (matcho/match (take-while some? (iterate (partial sut/get-prev-unit-value months nil) 12))
                  [12 11 10 9 8 7 6 5 4 3 2 1])

    (matcho/match (take 51 (iterate (partial sut/get-prev-unit-value years nil) 1970))
                  (range 1970 1920))

    (matcho/match (take-while some? (iterate (partial sut/get-prev-unit-value am-hours nil) 11))
                  [11 10 9 8 7 6 5 4 3 2 1 12]))

  (t/testing "ensure value"
    (t/is (= 1 (sut/ensure-unit (sut/unit-definition {:year 2020, :month 1, :day 0} :day)
                                {:year 2020, :month 1, :day 0}
                                0)))
    (t/is (= 29 (sut/ensure-unit (sut/unit-definition {:year 2020, :month 1, :day 30} :day)
                                 {:year 2020, :month 2, :day 30}
                                 30)))
    (t/is (= 28 (sut/ensure-unit (sut/unit-definition {:year 2020, :month 1, :day 30} :day)
                                 {:year 2019, :month 2, :day 30}
                                 30)))
    (t/is (= {:year 2020, :month 1, :day 1}
             (sut/ensure-less-significant-units {:year 2020, :month 1, :day 0} :month)))
    (t/is (= {:year 2020, :month 2, :day 29}
             (sut/ensure-less-significant-units {:year 2020, :month 2, :day 30} :month)))
    (t/is (= {:year 2019, :month 2, :day 28}
             (sut/ensure-less-significant-units {:year 2019, :month 2, :day 30} :month))))

  (t/testing "inc-unit"
    (def value ^::time/am-pm{:hour 12, :period :am})

    (matcho/match (take 24 (iterate (partial sut/inc-unit :hour) value))
                  [{:hour 12, :period :am} {:hour 1, :period :am} {:hour 2, :period :am} {:hour 3, :period :am} {:hour 4, :period :am} {:hour 5, :period :am} {:hour 6, :period :am} {:hour 7, :period :am} {:hour 8, :period :am} {:hour 9, :period :am} {:hour 10, :period :am} {:hour 11, :period :am}
                   {:hour 12, :period :pm} {:hour 1, :period :pm} {:hour 2, :period :pm} {:hour 3, :period :pm} {:hour 4, :period :pm} {:hour 5, :period :pm} {:hour 6, :period :pm} {:hour 7, :period :pm} {:hour 8, :period :pm} {:hour 9, :period :pm} {:hour 10, :period :pm} {:hour 11, :period :pm}])

    (def value ^::date/gregorian{:day 1, :month 1, :year 2020})
    (def calendar (->> value
                       (iterate (partial sut/inc-unit :day))
                       (take-while (comp #{2020} :year))
                       (partition-by :month)))
    (t/is (= 12 (count calendar)))
    (t/is (= 366 (count (flatten calendar)))))

  (t/testing "dec-unit"
    (def value ^::time/am-pm{:hour 11, :period :pm})

    (matcho/match (take 24 (iterate (partial sut/dec-unit :hour) value))
                  [{:hour 11, :period :pm} {:hour 10, :period :pm} {:hour 9, :period :pm} {:hour 8, :period :pm} {:hour 7, :period :pm} {:hour 6, :period :pm} {:hour 5, :period :pm} {:hour 4, :period :pm} {:hour 3, :period :pm} {:hour 2, :period :pm} {:hour 1, :period :pm} {:hour 12, :period :pm}
                   {:hour 11, :period :am} {:hour 10, :period :am} {:hour 9, :period :am} {:hour 8, :period :am} {:hour 7, :period :am} {:hour 6, :period :am} {:hour 5, :period :am} {:hour 4, :period :am} {:hour 3, :period :am} {:hour 2, :period :am} {:hour 1, :period :am} {:hour 12, :period :am}])

    (def value ^::date/gregorian{:day 31, :month 12, :year 2019})
    (def calendar (->> value
                       (iterate (partial sut/dec-unit :day))
                       (take-while (comp #{2019} :year))
                       (partition-by :month)))
    (t/is (= 12 (count calendar)))
    (t/is (= 365 (count (flatten calendar))))
    calendar)

  (t/testing "add-to-unit"
    (matcho/match (sut/add-to-unit :day {:day 1, :month 1, :year 2020} 9000)
                  {:day 22, :month 8, :year 2044})
    (matcho/match (sut/add-to-unit :day {:day 1, :month 1, :year 2020} -9000)
                  {:year 1995, :month 5, :day 12})
    (matcho/match (sut/add-to-unit :day {:day 1, :month 1, :year 2020} 0)
                  {:day 1, :month 1, :year 2020}))

  (t/testing "substract-from-unit"
    (matcho/match (sut/substract-from-unit :day {:day 1, :month 1, :year 2020} 9000)
                  {:year 1995, :month 5, :day 12})
    (matcho/match (sut/substract-from-unit :day {:day 1, :month 1, :year 2020} -9000)
                  {:day 22, :month 8, :year 2044})
    (matcho/match (sut/substract-from-unit :day {:day 1, :month 1, :year 2020} 0)
                  {:day 1, :month 1, :year 2020}))

  (t/testing "plus"
    (matcho/match (sut/plus {:day 1, :month 3, :year 2019})
                  {:day 1, :month 3, :year 2019})
    (matcho/match (sut/plus {:day 1, :month 3, :year 2019}
                            ^:delta{})
                  {:day 1, :month 3, :year 2019})
    (matcho/match (sut/plus {:day 1, :month 3, :year 2019}
                            ^:delta{:year 1, :day 1})
                  {:day 2, :year 2020})
    (matcho/match (sut/plus {:year 2021, :month 9, :day 3}
                            ^:delta{:month 2})
                  {:year 2021, :month 11, :day 3})
    (matcho/match (sut/plus {:year 2021, :month 9, :day 3}
                            ^:delta{:month -2})
                  {:year 2021, :month 7, :day 3})
    (matcho/match (sut/plus {:day 1, :month 3, :year 2019}
                            ^:delta{:day 99, :month -99, :year 0, :sec 30})
                  {:sec 30, :day 10, :month 3 :year 2011})
    (matcho/match (sut/plus {:day 1, :month 3, :year 2019}
                            ^:delta{:year 0}
                            ^:delta{:month -99}
                            ^:delta{:day 99}
                            ^:delta{:sec 30})
                  {:sec 30, :day 10, :month 3 :year 2011})
    (matcho/match (sut/plus {:day 31, :month 8, :year 2020}
                            ^:delta{:month 1})
                  {:day 30, :month 9, :year 2020})
    (t/is (sut/eq? {:year 2010 :month 12 :day 31 :hour 23 :min 30}
                   (sut/plus {:year 2011 :month 1 :day 1 :hour 0}
                             ^:delta{:min -30}))))

  (t/testing "invert"
    (matcho/match (sut/invert ^:delta{:day 1, :month 3})
                  ^:delta{:day -1, :month -3})
    (matcho/match (sut/invert ^:delta{:day 1, :month 3, :foo "bar"})
                  ^:delta{:day -1, :month -3, :foo "bar"}))

  (t/testing "difference"
    (matcho/match (sut/difference {:day 20, :month 7, :year 2020}
                                  {:day 26, :month 7, :year 2020})
                  ^:delta{:day 6})
    (matcho/match (sut/difference {:day 26, :month 7, :year 2020}
                                  {:day 20, :month 7, :year 2020})
                  ^:delta{:day 6})
    (matcho/match (sut/difference {:day 27, :month 7, :year 2020}
                                  {:day 5, :month 7, :year 1997})
                  ^:delta{:day 22, :year 23})
    (matcho/match (sut/difference {:day 5, :month 7, :year 1997}
                                  {:day 27, :month 7, :year 2020})
                  ^:delta{:day 22, :year 23})
    (matcho/match (sut/difference {:year 1} {:year -1})
                  ^:delta{:year 1})
    (matcho/match (sut/difference {:year -1} {:year 1})
                  ^:delta{:year 1})
    (t/is (sut/eq? ^:delta{:year 0}
                   (sut/difference {:day 27, :month 7, :year 2020}
                                   {:day 27, :month 7, :year 2020})))
    (t/is (sut/eq? ^:delta{:day 1}
                   (sut/difference {:day 28, :month 7, :year 2020}
                                   {:day 27, :month 7, :year 2020})))
    (t/is (sut/eq? ^{::time/military :delta}{:hour 0}
                   (sut/difference ^::time/military{:hour 12, :min 30, :tz {:hour -2}}
                                   ^::time/military{:hour 14, :min 30, :tz {:hour 0}}))))

  (t/testing "minus"
    (matcho/match (sut/minus ^:delta{:day 1, :month 3})
                  ^:delta{:day -1, :month -3})
    (matcho/match (sut/minus {:day 1, :month 3, :year 2019}
                             ^:delta{:day -99, :month 99, :year 0, :sec -30})
                  {:sec 30, :day 10, :month 3 :year 2011})
    (matcho/match (sut/minus {:day 1, :month 3, :year 2019}
                             ^:delta{:year 0}
                             ^:delta{:month 99}
                             ^:delta{:day -99}
                             ^:delta{:sec -30})
                  {:sec 30, :day 10, :month 3 :year 2011})

    (matcho/match (sut/minus {:day 20, :month 7, :year 2020}
                             {:day 26, :month 7, :year 2020})
                  ^:delta{:day -6})
    (matcho/match (sut/minus {:day 26, :month 7, :year 2020}
                             {:day 20, :month 7, :year 2020})
                  ^:delta{:day 6})
    (matcho/match (sut/minus {:day 27, :month 7, :year 2020}
                             {:day 5, :month 7, :year 1997})
                  ^:delta{:day 22, :year 23})
    (matcho/match (sut/minus {:day 5, :month 7, :year 1997}
                             {:day 27, :month 7, :year 2020})
                  ^:delta{:day -22, :year -23})
    (matcho/match (sut/minus {:year -1}
                             {:year 1})
                  ^:delta{:year -1})
    (matcho/match (sut/minus {:year 1}
                             {:year -1})
                  ^:delta{:year 1})
    (t/is (= (sut/minus {:day 27, :month 7, :year 2020}
                        {:day 26, :month 7, :year 2020})
             ^:delta{:day 1}))
    (matcho/match (sut/minus {:day 31, :month 7, :year 2020}
                            ^:delta{:month 1})
                  {:day 30, :month 6, :year 2020})
    (t/is (sut/eq? {:year 2020 :month 1 :day 31}
                   (sut/minus {:year 2020 :month 2} ^:delta{:day 1})))
    (t/is (sut/eq? ^:delta{:hour 2}
                   (sut/minus {:hour 3 :tz {:hour 2}}
                              {:hour 1 :tz {:hour 2}}))))

  (t/testing "delta"
    (matcho/match (sut/apply-delta {:year 2020, :month 7, :day 29, :hour 17, :min 20, :sec 50, :ms 733}
                                   ^:tz{:hour 2})
                  {:year 2020, :month 7, :day 29, :hour 19, :min 20, :sec 50, :ms 733, :tz {:hour 2}})
    (matcho/match (sut/apply-deltas {:year 2020, :month 7, :day 29, :hour 17, :min 20, :sec 50, :ms 733}
                                    [^:tz{:hour 2}])
                  {:year 2020, :month 7, :day 29, :hour 19, :min 20, :sec 50, :ms 733, :tz {:hour 2}})
    (matcho/match (sut/get-applied-deltas
                   (sut/apply-delta {:year 2020, :month 7, :day 29, :hour 17, :min 20, :sec 50, :ms 733}
                                    ^:tz{:hour 2}))
                  [{:hour 2}])
    (matcho/match (-> {:year 2020, :month 7, :day 29, :hour 17, :min 20, :sec 50, :ms 733}
                      (sut/apply-delta ^:tz{:hour 2})
                      sut/remove-deltas)
                  {:year 2020, :month 7, :day 29, :hour 17, :min 20, :sec 50, :ms 733})
    (matcho/match (-> {:year 2020, :month 7, :day 29, :hour 17, :min 20, :sec 50, :ms 733}
                      (sut/apply-deltas [^:tz{:hour 2}])
                      sut/drop-deltas)
                  {:year 2020, :month 7, :day 29, :hour 19, :min 20, :sec 50, :ms 733})
    (t/is (sut/eq? (sut/apply-deltas {:hour 20, :min 30} [nil {:hour 1} ^:tz{:hour 1}])
                   {:hour 22, :min 30, :delta ^:delta{:hour 1}, :tz ^:tz{:hour 1}})))

  (t/testing "eq?"
    (t/is (sut/eq? {:day 26, :month 7, :year 2020}))
    (t/is (sut/eq? {:day 26, :month 7, :year 2020} {:day 26, :month 7, :year 2020} {:day 26, :month 7, :year 2020}))
    (t/is (sut/eq? {} {}))
    (t/is (sut/not-eq? {} {:year 2020})))

  (t/testing "not-eq?"
    (t/is (not (sut/not-eq? {:day 26, :month 7, :year 2020})))
    (t/is (sut/not-eq? {:day 25, :month 7, :year 2020} {:day 26, :month 7, :year 2020} {:day 26, :month 7, :year 2020}))
    (t/is (sut/not-eq? {} {:day 26, :month 7, :year 2020})))

  (t/testing "lt?"
    (t/is (sut/lt? {:day 26, :month 7, :year 2020}))
    (t/is (sut/lt? {:day 26, :month 7, :year 2020} {:day 27, :month 7, :year 2020} {:day 28, :month 7, :year 2020}))
    (t/is (sut/lt? {} {:day 26, :month 7, :year 2020})))

  (t/testing "gt?"
    (t/is (sut/gt? {:day 26, :month 7, :year 2020}))
    (t/is (sut/gt? {:day 27, :month 7, :year 2020} {:day 26, :month 7, :year 2020} {:day 25, :month 7, :year 2020}))
    (t/is (sut/gt? {:day 26, :month 7, :year 2020} {})))

  (t/testing "lte?"
    (t/is (sut/lte? {:day 26, :month 7, :year 2020}))
    (t/is (sut/lte? {:day 26, :month 7, :year 2020} {:day 27, :month 7, :year 2020} {:day 27, :month 7, :year 2020}))
    (t/is (sut/lte? {} {:day 26, :month 7, :year 2020})))

  (t/testing "gte?"
    (t/is (sut/gte? {:day 26, :month 7, :year 2020}))
    (t/is (sut/gte? {:day 27, :month 7, :year 2020} {:day 26, :month 7, :year 2020} {:day 26, :month 7, :year 2020}))
    (t/is (sut/gte? {:day 26, :month 7, :year 2020} {})))

  (t/testing "cmp delta"
    (t/is (= 0 (sut/cmp ^{::time/military :tz}{:hour 20}
                        ^{::time/military :tz}{:hour 20, :min 0}))))

  (t/testing "cmp with delta"
    (t/is (= 0 (sut/cmp ^::time/military{:hour 20, :min 20, :tz {:hour 3}}
                        ^::time/military{:hour 19, :min 20, :tz {:hour 2}})))
    (t/is (= 0 (sut/cmp ^::time/military{:hour 19, :min 20, :tz {:hour 2}}
                        ^::time/military{:hour 20, :min 20, :tz {:hour 3}})))
    (t/is (= 0 (sut/cmp ^::time/military{:hour 20, :min 20, :tz {:hour 3}}
                        ^::time/military{:hour 20, :min 20, :tz {:hour 3}})))
    (t/is (= 0 (sut/cmp ^::time/military{:hour 20, :min 20, :tz {:hour 3}}
                        ^::time/military{:hour 20, :min 20})))
    (t/is (= 0 (sut/cmp ^::time/military{:hour 20, :min 20}
                        ^::time/military{:hour 20, :min 20, :tz {:hour 3}}))))

  (t/testing "normalize"
    (def d {:year 2020, :month 9, :day 3, :hour 2, :min 56, :sec 46, :ms 652, :tz {:hour 2}})
    (t/is (= d (sut/normalize d)))
    (t/is (= (sut/plus d ^:delta{:day 1337})
             (sut/normalize (update d :day + 1337))))
    (t/is (= ^:delta{:hour 1} (sut/normalize ^:delta{:min 60})))))


;;;;; Old ops tests:
;TODO: remove normalize calls in some cases

(t/deftest comparsion-operators-test
  (t/testing "="
    (t/is (sut/eq? {:year 2011 :month 1 :day 1 :hour 0}))
    (t/is (not (sut/eq? {:year 2011 :month 1 :day 2 :hour 0}
                      {:year 2011 :month 1 :day 1 :hour 0})))
    (t/is (sut/eq? {:year 2011 :month 1 :day 1 :hour 1}
                 {:year 2011 :month 1 :day 1 :hour 1}))
    (t/is (not (sut/eq? {:year 2011 :month 1 :day 1 :hour 0}
                      {:year 2011 :month 1 :day 2 :hour 0}
                      {:year 2011 :month 1 :day 1 :hour 0})))
    (t/is (sut/eq? {:year 2011 :month 1 :day 1 :hour 0}
                 {:year 2011 :month 1 :day 1 :hour 0}
                 {:year 2011 :month 1 :day 1 :hour 0})))
  (t/testing "= with utc-offset"
    (t/is (sut/eq? {:hour 10 :min 10 :tz {:hour 0}} {:hour 13 :min 10 :tz {:hour 3}}))
    (t/is (not (sut/eq? {:hour 13 :min 10 :tz {:hour 0}} {:hour 13 :min 10 :tz {:hour 3}})))
    (t/is (sut/eq? (sut/normalize {:hour 210 :min 10 :tz {:hour 0}})
                   (sut/normalize {:hour 213 :min 10 :tz {:hour 3}})))
    (t/is (sut/eq? {:hour 15 :min 10 :tz {:hour 0}} {:hour 13 :min 10 :tz {:hour -2}}))
    (t/is (sut/eq? {:hour 12 :min 10 :tz {:hour 2}} {:hour 13 :min 10 :tz {:hour 3}}))
    (t/is (sut/eq? {:hour 10 :min 10 :tz {:hour 0}} {:hour 12 :min 10 :tz {:hour 2}}))
    (t/is (sut/eq? {:hour 12 :min 10} {:hour 12 :min 10 :tz {:hour 2}})))
  (t/testing "not="
    (t/is (not (sut/not-eq? {:year 2011 :month 1 :day 1 :hour 0})))
    (t/is (not (sut/not-eq? {:year 2011 :month 1 :day 1 :hour 0}
                          {:year 2011 :month 1 :day 1 :hour 0})))
    (t/is (sut/not-eq? {:year 2011 :month 1 :day 2 :hour 1}
                     {:year 2011 :month 1 :day 1 :hour 1}))
    (t/is (not (sut/not-eq? {:year 2011 :month 1 :day 1 :hour 0}
                          {:year 2011 :month 1 :day 1 :hour 0}
                          {:year 2011 :month 1 :day 1 :hour 0})))
    (t/is (sut/not-eq? {:year 2011 :month 1 :day 1 :hour 0}
                     {:year 2011 :month 1 :day 2 :hour 0}
                     {:year 2011 :month 1 :day 1 :hour 0})))
  (t/testing "not= with utc-offset"
    (t/is (not (sut/not-eq? {:hour 10 :min 10 :tz {:hour 0}} {:hour 13 :min 10 :tz {:hour 3}})))
    (t/is (sut/not-eq? {:hour 13 :min 10 :tz {:hour 0}} {:hour 13 :min 10 :tz {:hour 3}}))
    (t/is (not (sut/not-eq? (sut/normalize {:hour 210 :min 10 :tz {:hour 0}})
                            (sut/normalize {:hour 213 :min 10 :tz {:hour 3}}))))
    (t/is (not (sut/not-eq? {:hour 15 :min 10 :tz {:hour 0}} {:hour 13 :min 10 :tz {:hour -2}})))
    (t/is (not (sut/not-eq? {:hour 12 :min 10 :tz {:hour 2}} {:hour 13 :min 10 :tz {:hour 3}})))
    (t/is (not (sut/not-eq? {:hour 10 :min 10 :tz {:hour 0}} {:hour 12 :min 10 :tz {:hour 2}}))))
  (t/testing ">"
    (t/is (sut/gt? {:year 2011 :month 1 :day 1 :hour 0}))
    (t/is (sut/gt? (sut/normalize {:min 120}) (sut/normalize {:hour 1})))
    (t/is (not (sut/gt? {:year 2011 :month 1 :day 1 :hour 0}
                     {:year 2011 :month 1 :day 2 :hour 0})))
    (t/is (sut/gt? {:year 2011 :month 1 :day 2 :hour 0}
                {:year 2011 :month 1 :day 1 :hour 0}))
    (t/is (sut/gt? {:year 2011 :month 1 :day 2 :hour 0}
                {:year 2011 :month 1 :day 1 :hour 1}
                {:year 2011 :month 1 :day 1 :hour 0})))
  (t/testing "> with utc-offset"
    (t/is (sut/gt? {:hour 13 :tz {:hour 0}} {:hour 13 :tz {:hour 3}}))
    (t/is (not (sut/gt? {:hour 13 :tz {:hour 3}} {:hour 13 :tz {:hour 0}})))
    (t/is (sut/gt? {:hour 13 :tz {:hour -3}} {:hour 13 :tz {:hour 0}}))
    (t/is (not (sut/gt? {:hour 13 :tz {:hour 0}} {:hour 13 :tz {:hour -3}})))
    (t/is (sut/gt? {:hour 13 :tz {:hour -3}} {:hour 13 :tz {:hour -2}}))
    (t/is (not (sut/gt? {:hour 13 :tz {:hour -2}} {:hour 13 :tz {:hour -3}})))
    (t/is (not (sut/gt? {:hour 10 :min 10 :tz {:hour 0}} {:hour 13 :min 10 :tz {:hour 3}}))))
  (t/testing "<"
    (t/is (sut/lt? {:year 2011 :month 1 :day 1 :hour 0}))
    (t/is (sut/lt? (sut/normalize {:hour 1}) (sut/normalize {:min 120})))
    (t/is (not (sut/lt? {:year 2011 :month 1 :day 2 :hour 0}
                     {:year 2011 :month 1 :day 1 :hour 0})))
    (t/is (sut/lt? {:year 2011 :month 1 :day 1 :hour 0}
                {:year 2011 :month 1 :day 2 :hour 0}))
    (t/is (sut/lt? {:year 2011 :month 1 :day 1 :hour 0}
                {:year 2011 :month 1 :day 1 :hour 1}
                {:year 2011 :month 1 :day 2 :hour 0})))
  (t/testing "< with utc-offset"
    (t/is (sut/lt? {:hour 13 :tz {:hour 3}} {:hour 13 :tz {:hour 0}}))
    (t/is (not (sut/lt? {:hour 13 :tz {:hour 0}} {:hour 13 :tz {:hour 3}})))
    (t/is (sut/lt? {:hour 13 :tz {:hour 0}} {:hour 13 :tz {:hour -3}}))
    (t/is (not (sut/lt? {:hour 13 :tz {:hour -3}} {:hour 13 :tz {:hour 0}})))
    (t/is (sut/lt? {:hour 13 :tz {:hour -2}} {:hour 13 :tz {:hour -3}}))
    (t/is (not (sut/lt? {:hour 13 :tz {:hour -3}} {:hour 13 :tz {:hour -2}})))
    (t/is (not (sut/lt? {:hour 13 :min 10 :tz {:hour 3}} {:hour 10 :min 10 :tz {:hour 0}}))))
  (t/testing "<="
    (t/is (sut/lte? {:year 2011 :month 1 :day 1 :hour 0}))
    (t/is (sut/lte? {:year 2011 :month 1 :day 1 :hour 0}
                 {:year 2011 :month 1 :day 2 :hour 0}))
    (t/is (sut/lte? {:year 2011 :month 1 :day 1 :hour 0}
                 {:year 2011 :month 1 :day 1 :hour 0}))
    (t/is (not (sut/lte? {:year 2011 :month 1 :day 2 :hour 0}
                      {:year 2011 :month 1 :day 1 :hour 0})))
    (t/is (sut/lte? {:year 2011 :month 1 :day 1 :hour 0}
                 {:year 2011 :month 1 :day 1 :hour 0}
                 {:year 2011 :month 1 :day 2 :hour 0})))
  (t/testing ">="
    (t/is (sut/gte? {:year 2011 :month 1 :day 1 :hour 0}))
    (t/is (sut/gte? {:year 2011 :month 1 :day 2 :hour 0}
                 {:year 2011 :month 1 :day 1 :hour 0}))
    (t/is (sut/gte? {:year 2011 :month 1 :day 2 :hour 1}
                 {:year 2011 :month 1 :day 1 :hour 1}
                 {:year 2011 :month 1 :day 1 :hour 0}))
    (t/is (not (sut/gte? {:year 2011 :month 1 :day 1 :hour 0}
                      {:year 2011 :month 1 :day 2 :hour 0})))
    (t/is (sut/gte? {:year 2011 :month 1 :day 2 :hour 0}
                 {:year 2011 :month 1 :day 1 :hour 0}
                 {:year 2011 :month 1 :day 1 :hour 0}))))

(t/deftest arithmetic-operations-test
  (t/testing "+"
    (def t
      {:year  2018
       :month 1
       :day   1
       :hour  12
       :min   30
       :sec   30
       :ms    500})

    (matcho/match (sut/plus t ^:delta{:ms 200})
                  {:ms 700})

    (matcho/match (sut/plus t ^:delta{:ms 600})
                  {:ms 100, :sec 31})

    (t/is (= {:ms 500, :sec 1}
             (sut/plus ^:delta{:ms 600} ^:delta{:ms 600} ^:delta{:ms 300})))

    (matcho/match (sut/plus t ^:delta{:sec 20})
                  {:sec 50})

    (matcho/match (sut/plus t ^:delta{:sec 20})
                  {:sec 50})

    (matcho/match (sut/plus t ^:delta{:min 20})
                  {:hour 12, :min 50})

    (matcho/match (sut/plus t ^:delta{:min 30})
                  {:hour 13})

    (t/is (= {:year 2019 :month 1 :day 1}
             (sut/plus {:year 2018 :month 12 :day 31} ^:delta{:day 1})))

    (t/is (= {:year 2018 :month 2 :day 1}
             (sut/plus {:year 2018 :month 1 :day 1} ^:delta{:day 31})))

    (t/is (= {:year 2020 :month 1 :day 1}
             (sut/plus {:year 2018 :month 12 :day 31} ^:delta{:day 366})))

    (t/is (= {:year 2018 :month 3 :day 1}
             (sut/plus {:year 2018 :month 2 :day 28} ^:delta{:day 1})))

    (t/is (= {:year 2018 :month 3 :day 31}
             (sut/plus {:year 2018 :month 3 :day 30} ^:delta{:day 1})))

    (t/is (= {:year 2018 :month 4 :day 1}
             (sut/plus {:year 2018 :month 3 :day 31} ^:delta{:day 1})))

    (t/is (= {:ms 400}
             (sut/plus {:ms 100} ^:delta{:ms 300})))

    (t/is (= {:ms 200 :sec 1}
             (sut/plus {:ms 900} ^:delta{:ms 300})))

    (t/is (= {:sec 30 :min 1}
             (sut/plus {:sec 40} ^:delta{:sec 50})))

    (t/is (= {:min 30 :hour 1}
             (sut/plus {:min 40} ^:delta{:min 50})))

    (t/is (= {:hour 3 :day 1}
             (sut/plus ^:delta{:hour 13} ^:delta{:hour 14})))

    (t/is (= {:year 2011 :month 1 :day 2 :hour 4}
             (sut/plus {:year 2011 :month 1 :day 1 :hour 23} ^:delta{:hour 5})))

    (t/is (= {:year 2011 :month 2 :day 2}
             (sut/plus {:year 2011 :month 1 :day 30} ^:delta{:day 3})))

    (t/is (= {:year 2012 :month 1 :day 1}
             (sut/plus {:year 2011 :month 1 :day 1} ^:delta{:day 365})))

    (t/is (= {:year 2012 :month 1 :day 1 :hour 4}
             (sut/plus {:year 2011 :month 12 :day 31 :hour 23} ^:delta{:hour 5})))

    (t/is (= {:year 2010 :month 12 :day 31 :hour 23}
             (sut/plus {:year 2011 :month 1 :day 1 :hour 0} ^:delta{:hour -1})))

    (t/is (= {:year 2010 :month 12 :day 31 :hour 23 :min 59 :sec 59}
             (sut/plus {:year 2011 :month 1 :day 1 :hour 0} ^:delta{:sec -1})))

    (t/is (= {:year 2010 :month 12 :day 31 :hour 23 :min 59 :sec 59 :ms 999}
             (sut/plus {:year 2011 :month 1 :day 1 :hour 0} ^:delta{:ms -1})))

    (t/is (= {:year 2010 :month 12 :day 31 :hour 23 :min 30}
             (sut/plus {:year 2011 :month 1 :day 1 :hour 23} ^:delta{:hour -23 :min -30})))

    (t/is (= {:year 2019 :month 12 :day 1}
             (sut/plus {:year 2019 :month 11 :day 1} ^:delta{:month 1})))

    (t/is (= {:year 2020 :month 1 :day 1}
             (sut/plus {:year 2019 :month 11 :day 1} ^:delta{:month 2})))

    (t/is (= {:year 2020 :month 1 :day 1}
             (sut/plus {:year 2019 :month 12 :day 1} ^:delta{:month 1})))

    (t/is (= {:year 2019 :month 12 :day 31}
             (sut/plus {:year 2019 :month 11 :day 31} ^:delta{:month 1})))

    (t/is (= {:year 2020 :month 2}
             (sut/plus {:year 2020 :month 2} ^:delta{:day 0})))

    (t/is (= {:year 2019, :month 12, :day 10, :hour 15, :min 17, :sec 50, :ms 911}
             (sut/plus {:year 2019, :month 12, :day 10, :hour 13, :min 17, :sec 50, :ms 911} ^:delta{:hour 2})))

    (t/is (= {:hour 14 :tz {:hour 2}}
             (sut/plus {:hour 4 :tz {:hour 2}} ^:delta{:hour 10})))

    #_(t/is (sut/eq? {:hour 0, :day 1, :tz {:hour -2}}
                 (sut/plus {:day 1 :hour 23 :tz {:hour -2}} ^:delta{:hour 1})))

    (t/is (= {:hour 2 :tz {:hour -2}}
             (sut/plus {:hour 1 :tz {:hour -2}} ^:delta{:hour 1})))

    #_(t/testing "with custom units"
      (def normalize-ns (sut/gen-norm :ns :ms 1000000 0 0))
      (defmethod sut/normalize-rule :ns [_ t] (normalize-ns t))

      (t/is (= (sut/plus {:ns 10} {:ns 1})
             {:ns 11}))

      (t/is (= (sut/plus {:ns 999999999} {:ns 1})
             {:sec 1}))

      (t/is (= (sut/plus {:ns 9999999} {:ns 999000001})
             {:sec 1 :ms 9}))

      (t/is (= (sut/plus {:year 2019 :month 12 :day 31 :hour 23 :min 59 :sec 59 :ns 999999999} {:ns 1})
             {:year 2020 :month 1 :day 1}))))

  (t/testing "-"
    (t/is (= {:year 2016, :month 1, :day 1, :hour 23, :min 30}
             (sut/minus {:year 2016, :month 12, :day 31, :hour 23, :min 30} ^:delta{:day 365})))

    (t/is (= {:year 2015, :month 12, :day 31, :hour 23, :min 30}
             (sut/minus {:year 2016 :month 12 :day 31 :hour 23 :min 30} ^:delta{:day 366})))

    (t/is (= {:year 2020 :month 1 :day 31}
             (sut/minus {:year 2020 :month 2}
                        ^:delta{:day 1})))
    (t/is (= {:year 2020 :month 2}
             (sut/minus {:year 2020 :month 2}
                        ^:delta{:day 0})))

    (t/is (sut/eq? {:hour 0, :tz {:hour -2}}
                   (sut/minus {:hour 2 :tz {:hour -2}} ^:delta{:hour 2})))
    (t/is (sut/eq? ^:delta{:hour 0}
                   (sut/minus {:hour 2 :tz {:hour -2}} {:hour 2})))
    (t/is (sut/eq? ^:delta{:hour 2}
                   (sut/minus {:hour 3 :tz {:hour 2}} {:hour 1 :tz {:hour 2}})))))

(t/deftest normalize-test
  (t/is (= {:year 2019 :month 11 :day 10}
         (sut/normalize {:year 2019 :month 11 :day 10})))
  (t/is (= {:year 2019 :month 12 :day 10}
         (sut/normalize {:year 2019 :month 12 :day 10})))
  (t/is (= {:year 2020 :month 12 :day 10}
         (sut/normalize {:year 2019 :month 24 :day 10})))
  (t/is (= {:year 2021 :month 1 :day 10}
         (sut/normalize {:year 2019 :month 25 :day 10}))))

(t/deftest tz-operations-test
  (t/testing "to-utc"
    (t/is (= {:hour 10 :min 10 :tz {:hour 0}}
             (sut/to-delta {:hour 10 :min 10} ^:tz{:hour 0})))

    (t/is (= {:hour 10 :min 10 :tz {:hour 0}}
             (sut/to-delta {:hour 13 :min 10 :tz {:hour 3}} ^:tz{:hour 0})))

    (t/is (= {:hour 10 :min 10 :tz {:hour 0}}
             (sut/to-delta {:hour 7 :min 10 :tz {:hour -3}} ^:tz{:hour 0})))

    (t/is (sut/eq? {:hour 0 :min 10 :tz {:hour 0}}
                   (sut/to-delta (sut/normalize {:min 130 :tz {:hour 2}}) ^:tz{:hour 0})))

    #_(t/is (= {:day -1 :hour 23 :tz {:hour 0}}
               (sut/to-delta {:hour 1 :tz {:hour 2}} ^:tz{:hour 0})))

    #_(t/is (= {:day 1 :hour 1 :tz {:hour 0}}
               (sut/to-delta {:hour 23 :tz {:hour -2}} ^:tz{:hour 0})))

    (t/is (sut/eq? {:hour 0 :tz {:hour 0}}
                   (sut/to-delta {:hour 1 :tz {:hour 1}} ^:tz{:hour 0}))))

  (t/testing "to-delta"
    (t/is (= {:hour 10 :min 10}
             (sut/to-delta {:hour 10 :min 10} nil)))

    (t/is (= {:hour 13 :min 10 :tz {:hour 3}}
             (sut/to-delta {:hour 10 :min 10 :tz {:hour 0}} ^:tz{:hour 3})))

    (t/is (= {:hour 10 :min 10 :tz {:hour 3}}
             (sut/to-delta {:hour 10 :min 10} ^:tz{:hour 3})))

    (t/is (= {:hour 7 :min 10 :tz {:hour -3}}
             (sut/to-delta {:hour 10 :min 10 :tz {:hour 0}} ^:tz{:hour -3})))

    (t/is (= {:hour 10 :min 10 :tz {:hour -3}}
             (sut/to-delta {:hour 10 :min 10} ^:tz{:hour -3})))

    (t/is (= {:hour 2 :min 10 :tz {:hour 2}}
             (sut/to-delta {:min 10 :tz {:hour 0}} ^:tz{:hour 2})))

    (t/is (= {:min 10 :tz {:hour 2}}
             (sut/to-delta {:min 10} ^:tz{:hour 2})))

    (t/is (= {:day 1 :hour 23 :tz {:hour -2}}
             (sut/to-delta {:day 2 :hour 1 :tz {:hour 0}} ^:tz{:hour -2})))

    (t/is (= {:day 1 :hour 1 :tz {:hour -2}}
             (sut/to-delta {:day 1 :hour 1} ^:tz{:hour -2})))

    (t/is (sut/eq? (sut/to-delta {:hour 1 :tz {:hour 0}} ^:tz{:hour -1})
                   {:hour 0, :tz {:hour -1}}))

    (t/is (= {:hour 1 :tz {:hour -1}}
             (sut/to-delta {:hour 1} ^:tz{:hour -1})))))

#_(t/deftest test-timezones
  (t/testing "tz comparsion"
    (t/is (sut/lte {:year 2018 :month 5 :day 2 :hour 14 :tz :ny}
                 {:year 2018 :month 5 :day 2 :hour 14 :tz :ny}))

    (t/is (sut/lte {:year 2018 :month 5 :day 2 :hour 14 :tz :ny}
                 {:year 2018 :month 5 :day 2 :hour 14 :sec 1 :tz :ny}))

    (t/is (sut/gt {:year 2018 :month 5 :day 2 :hour 14 :sec 1 :tz :ny}
                {:year 2018 :month 5 :day 2 :hour 14 :tz :ny}))

    (t/is (sut/gt {:year 2018 :month 5 :day 2 :hour 13 :min 120 :tz :ny}
                {:year 2018 :month 5 :day 2 :hour 14 :tz :ny}))

    (t/is (sut/lte {:year 2018 :month 5 :day 2 :hour 14 :tz :ny}
                 {:year 2018 :month 5 :day 2 :hour 13 :min 120 :tz :ny}))

    (t/is (sut/lte {:year 2018 :month 5 :day 2 :hour 14 :tz :ny}
                 {:year 2018 :month 11}))

    (t/is (sut/gt {:year 2018 :month 11}
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

  (t/is (= (sut/to-delta {:year 2018 :month 5 :day 2 :hour 14 :tz :ny} ^:tz{:hour 0})
         {:year 2018 :month 5 :day 2 :hour 18 :tz {:hour 0}}))

  (t/is (= (sut/to-delta {:year 2018 :month 5 :day 2 :hour 18 :tz {:hour 0}} :ny)
         {:year 2018 :month 5 :day 2 :hour 14 :tz :ny}))

  (t/is (= (sut/to-delta {:year 2018 :month 5 :day 2 :hour 18} :ny)
         {:year 2018 :month 5 :day 2 :hour 18 :tz :ny}))

  (t/is (= (sut/to-delta {:year 2018 :month 2 :day 2 :hour 14 :tz :ny} ^:tz{:hour 0})
         {:year 2018 :month 2 :day 2 :hour 19 :tz {:hour 0}}))

  (t/is (= (sut/to-delta {:year 2018 :month 2 :day 2 :hour 19 :tz {:hour 0}} :ny)
         {:year 2018 :month 2 :day 2 :hour 14 :tz :ny}))

  (t/is (= (sut/to-delta {:year 2018 :month 2 :day 2 :hour 19} :ny)
         {:year 2018 :month 2 :day 2 :hour 19 :tz :ny})))
