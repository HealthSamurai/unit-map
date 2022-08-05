(ns unit-map.impl.ops-test
  (:require [unit-map.impl.ops :as sut]
            [unit-map.core :as umap]
            [clojure.test :as t]))


(def treg_ (umap/new-registry))

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

  (umap/reg-useq! treg_ :unit :hour,   :useq #unit-map/useq[0 1 .. 23] :next-unit :day)

  (umap/reg-useq! treg_ :unit :am-pm/hour,   :useq #unit-map/useq[12 1 2 .. 11] :next-unit :am-pm/period, :eq-unit :hour)
  (umap/reg-useq! treg_ :unit :am-pm/period, :useq #unit-map/useq[:am :pm])

  (umap/reg-useq! treg_ :unit :day,   :useq #unit-map/useq[1 2 .. days-in-month] :next-unit :month)
  (umap/reg-useq! treg_ :unit :month, :useq #unit-map/useq[:jan :feb  :mar :apr :may  :jun :jul :aug  :sep :oct :nov  :dec] :next-unit :year)
  (umap/reg-useq! treg_ :unit :year,  :useq #unit-map/useq[##-Inf .. -2 -1 1 2 .. ##Inf])

  (umap/reg-useq! treg_
                 :unit :epoch-year
                 :useq #unit-map/useq[(fn [{:keys [epoch]}]
                                        (if (= :BC epoch) ##Inf 1))
                                      (fn [{:keys [epoch]}]
                                        (if (= :BC epoch) -1 1))
                                      ..
                                      (fn [{:keys [epoch]}]
                                        (if (= :BC epoch) 1 ##Inf))]
                 :next-unit :epoch
                 :eq-unit :year)

  (umap/reg-useq! treg_ :unit :epoch,  :useq #unit-map/useq[:BC :AD])

  (def datetime   (umap/reg-system! treg_ [:hour :day :month :year]))
  (def date       (umap/reg-system! treg_ [:day :month :year]))
  (def month-year (umap/reg-system! treg_ [:month :year]))
  (def year-epoch (umap/reg-system! treg_ [:epoch-year :epoch]))
  (def am-pm-time (umap/reg-system! treg_ [:am-pm/hour :am-pm/period])))


(t/deftest arithmetic-test
  (t/testing "inc-unit"
    (t/testing "am-pm clock"
      (def value {:am-pm/hour 12, :am-pm/period :am})

      (t/is (= [{:am-pm/hour 12, :am-pm/period :am} {:am-pm/hour 1, :am-pm/period :am} {:am-pm/hour 2, :am-pm/period :am}
                {:am-pm/hour 3, :am-pm/period :am} {:am-pm/hour 4, :am-pm/period :am} {:am-pm/hour 5, :am-pm/period :am}
                {:am-pm/hour 6, :am-pm/period :am} {:am-pm/hour 7, :am-pm/period :am} {:am-pm/hour 8, :am-pm/period :am}
                {:am-pm/hour 9, :am-pm/period :am} {:am-pm/hour 10, :am-pm/period :am} {:am-pm/hour 11, :am-pm/period :am}

                {:am-pm/hour 12, :am-pm/period :pm} {:am-pm/hour 1, :am-pm/period :pm} {:am-pm/hour 2, :am-pm/period :pm}
                {:am-pm/hour 3, :am-pm/period :pm} {:am-pm/hour 4, :am-pm/period :pm} {:am-pm/hour 5, :am-pm/period :pm}
                {:am-pm/hour 6, :am-pm/period :pm} {:am-pm/hour 7, :am-pm/period :pm} {:am-pm/hour 8, :am-pm/period :pm}
                {:am-pm/hour 9, :am-pm/period :pm} {:am-pm/hour 10, :am-pm/period :pm} {:am-pm/hour 11, :am-pm/period :pm}]
               (take 24 (iterate (partial sut/inc-unit @treg_ :am-pm/hour) value)))))

    (t/testing "epoch"
      (t/is (= [{:year -2} {:year -1} {:year 1} {:year 2}]
               (take 4 (iterate #(sut/inc-unit @treg_ :year %) {:year -2}))))

      (t/is (= [{:epoch :BC, :epoch-year 2} {:epoch :BC, :epoch-year 1}
                {:epoch :AD, :epoch-year 1} {:epoch :AD, :epoch-year 2}]
               (take 4 (iterate #(sut/inc-unit @treg_ :epoch-year %) {:epoch :BC :epoch-year 2})))))

    (t/testing "calendar"
      (def value {:day 1, :month :jan, :year 2020})

      (def calendar (->> value
                         (iterate (partial sut/inc-unit @treg_ :day))
                         (take-while (comp #{2020} :year))
                         (partition-by :month)))

      (t/is (= 12 (count calendar)))
      (t/is (= 366 (count (flatten calendar))))))

  (t/testing "dec-unit"
    (t/testing "am-pm clock"
      (def value {:am-pm/hour 11, :am-pm/period :pm})

      (t/is (= [{:am-pm/hour 11, :am-pm/period :pm} {:am-pm/hour 10, :am-pm/period :pm} {:am-pm/hour 9, :am-pm/period :pm}
                {:am-pm/hour 8, :am-pm/period :pm} {:am-pm/hour 7, :am-pm/period :pm} {:am-pm/hour 6, :am-pm/period :pm}
                {:am-pm/hour 5, :am-pm/period :pm} {:am-pm/hour 4, :am-pm/period :pm} {:am-pm/hour 3, :am-pm/period :pm}
                {:am-pm/hour 2, :am-pm/period :pm} {:am-pm/hour 1, :am-pm/period :pm} {:am-pm/hour 12, :am-pm/period :pm}

                {:am-pm/hour 11, :am-pm/period :am} {:am-pm/hour 10, :am-pm/period :am} {:am-pm/hour 9, :am-pm/period :am}
                {:am-pm/hour 8, :am-pm/period :am} {:am-pm/hour 7, :am-pm/period :am} {:am-pm/hour 6, :am-pm/period :am}
                {:am-pm/hour 5, :am-pm/period :am} {:am-pm/hour 4, :am-pm/period :am} {:am-pm/hour 3, :am-pm/period :am}
                {:am-pm/hour 2, :am-pm/period :am} {:am-pm/hour 1, :am-pm/period :am} {:am-pm/hour 12, :am-pm/period :am}]
               (take 24 (iterate (partial sut/dec-unit @treg_ :am-pm/hour) value)))))

    (t/testing "calendar"
      (def value {:day 31, :month :dec, :year 2019})

      (def calendar (->> value
                         (iterate (partial sut/dec-unit @treg_ :day))
                         (take-while (comp #{2019} :year))
                         (partition-by :month)))

      (t/is (= 12 (count calendar)))
      (t/is (= 365 (count (flatten calendar))))))

  (t/testing "add-to-unit"
    (t/is (= {:hour 0, :day 22, :month :aug, :year 2044}
             (sut/add-to-unit @treg_ {:hour 0 :day 1, :month :jan, :year 2020} :hour 216000)))
    (t/is (= {:hour 0 ,:year 1995, :month :may, :day 12}
             (sut/add-to-unit @treg_ {:hour 0 :day 1, :month :jan, :year 2020} :hour -216000)))
    (t/is (= {:day 1, :month :jan, :year 2020}
             (sut/add-to-unit @treg_ {:day 1, :month :jan, :year 2020} :hour 0))))

  (t/testing "subtract-from-unit"
    (t/is (= {:hour 0, :day 22, :month :aug, :year 2044}
             (sut/subtract-from-unit @treg_ {:day 1, :month :jan, :year 2020} :hour -216000)))
    (t/is (= {:hour 0,:year 1995, :month :may, :day 12}
             (sut/subtract-from-unit @treg_ {:day 1, :month :jan, :year 2020} :hour 216000)))
    (t/is (= {:day 1, :month :jan, :year 2020}
             (sut/subtract-from-unit @treg_ {:day 1, :month :jan, :year 2020} :hour 0)))))


(comment
  (def treg_ unit-map.impl.system-test/treg_)

 (assoc-delta {:hour 17} :utc {:hours 3})        ;; => {:hour 17, :utc {:hours 3}}
 (dissoc-delta {:hour 17, :utc {:hours 3}} :utc) ;; => {:hour 17}

 (apply-delta {:hour 17} :utc {:hours 3})        ;; => {:hour 20, :utc {:hours 3}}
 (remove-delta {:hour 20, :utc {:hours 3}} :utc) ;; => {:hour 17}

 (to-delta {:hour 20, :utc {:hours 3}} :utc {:hour 2}) ;; => {:hour 16, :utc {:hour 2}}

 (dissoc-deltas {:hour 20, :utc {:hours 3}}) ;; => {:hour 20}
 (remove-deltas {:hour 20, :utc {:hours 3}}) ;; => {:hour 17}

 "+ delta = delta
  + value = value

     a   + delta =   a
   delta +   a   =   a
   value + value = error"

 ;; forbid not normalized values?
 ;; (- {:months 1} {:days 4}) ;; => {:months 1, :days -4}
 ;; maybe deltas must consist of one unit

 (+ {:year 2020, :month :feb}) ;; => {:year 2020, :month :feb}
 (+ {:months 2})               ;; => {:year 2020, :month :apr}

 (+ {:year 2020, :month :feb} {:months 2}) ;; => {:year 2020, :month :apr}
 (+ {:months 2} {:year 2020, :month :feb}) ;; => {:year 2020, :month :apr}
 (+ {:months 2} {:months 2})               ;; => {:months 4}
 (+ {:year 2020, :month :feb} {:year 2020, :month :feb}) ;; => error

 (+ {:hour 17} {:hours 3})                  ;; => {:hour 20}
 (+ {:hour 17, :utc {:hours 3}} {:hours 3}) ;; => {:hour 20, :utc {:hours 3}}

 "- delta = delta
  - value = error

    a   -   a   = delta
  value - delta = value
  delta - value = error"

 (- {:year 2020, :month :feb}) ;; => error
 (- {:months 2})               ;; => {:months -2}

 (abs {:year 2020, :month :feb}) ;; ?> {:year 2020, :month :feb}
 (abs {:months -2 :days -4})     ;; => {:months 2, :days 4}
 (abs {:months 2 :days 4})       ;; => {:months 2, :days 4}
 (abs {:months -2 :days 4})      ;; ?> {:months 2 :days -4}
 (abs {:months 2 :days -4})      ;; ?> {:months 2 :days -4}

 (normalize {:year 2020, :month :feb, :day 30}) ;; {:year 2020, :month :mar, :day 2}
 (normalize {:year 2020, :month :feb, :day 30, :hour -100}) ;; {:year 2020, :month :mar, :day 2}
 (normalize {:months 20, :days 35, :hours 100}) ;; ?> {:years 1, :month 8, :days 39, :hours 4}
 (normalize {:hours 100}) ;; => {:days 4, :hours 4}

 (normalize {:inches 16}) ;; => {:feets 1, :inches 4}
 (normalize {:inch 16})   ;; => {:feet 1, :inch 4}

 (- {:year 2020, :month :feb} {:months 2} {:months 2}) ;; => {:year 2019, :month :oct}

 (- {:months 2} {:year 2020, :month :feb} {:months 2}) ;; => error
 (- {:months 2} {:months 2} {:year 2020, :month :feb}) ;; => error

 (- {:months 2} {:months 2} {:months 2})               ;; => {:months -2}

 (- {:year 2020, :month :feb} {:year 2020, :month :mar}) ;; => {:months -1}
 (- {:year 2020, :month :mar} {:year 2020, :month :feb}) ;; => {:months 1}

 (- {:year 2020, :month :feb} {:year 2020, :month :mar} {:year 2020, :month :mar}) ;; => error

 (- {:months 1} {:days 4}) ;; => {:months 1, :days -4}

 (difference {:year 2020, :month :feb} {:months 2}) ;; => error
 (difference {:months 2} {:year 2020, :month :feb}) ;; => error

 (difference {:months 1} {:months 2}) ;; => {:months 1}
 (difference {:months 2} {:months 1}) ;; => {:months 1}
 (difference {:months 2} {:months 2}) ;; => {:months 0}

 (difference {:year 2020, :month :feb} {:year 2020, :month :mar}) ;; => {:years 0, months 1}
 (difference {:year 2020, :month :mar} {:year 2020, :month :feb}) ;; => {:years 0, months 1}
 (difference {:year 2020, :month :feb} {:year 2020, :month :feb}) ;; => {:years 0, :months 0}

 (difference-in [:days] {:year 2020, :month :feb, :day 1} {:year 2020, :month :mar, :day 1}) ;; => {:days 28}
 (difference-in [:days] {:year 2020, :month :mar, :day 1} {:year 2020, :month :feb, :day 1}) ;; => {:days -28}
 (difference-in [:days] {:year 2020, :month :feb, :day 1} {:year 2020, :month :feb, :day 1}) ;; => {:days 0}

 (strip-zeros {:years 0, :months 1}) ;; => {:months 1}

 (compare {:hour 17} {:hour 16}) ;; => 1
 (compare {:hour 17} {:hour 16}) ;; => 1
 (compare {:hour 17, :utc {:hours 3}}
          {:hour 16, :utc {:hours 2}}) ;; => 0
 (compare (remove-deltas {:hour 17, :utc {:hours 3}})
          (remove-deltas {:hour 16, :utc {:hours 2}})) ;; => 1

 (- {:hour 17} {:hour 3})   ;; => {:hours 14}
 (- {:hour 17} {:hours 3})  ;; => {:hour 14}
 (- {:hours 17} {:hours 3}) ;; => {:hours 14}
 (- {:hour 17, :utc {:hours 3}} {:hours 3}) ;; => {:hour 14, :utc {:hour 3}}

 (- {:hour 17} {:hour 16}) ;; => {:hours 1}
 (- {:hour 16} {:hour 17}) ;; => {:hours -1}

 (- {:hour 17, :utc {:hours 3}} {:hour 16, :utc {:hours 2}}) ;; => {:hours 0}
 (- {:hour 17, :utc {:hours 3}} {:hour 16, :utc {:hours 3}}) ;; => {:hours 1}
 (- {:hour 16, :utc {:hours 3}} {:hour 17, :utc {:hours 3}}) ;; => {:hours -1}

 (difference {:hour 17} {:hour 16}) ;; => {:hours -1}
 (difference {:hour 16} {:hour 17}) ;; => {:hours 1}

 (difference {:hours 17} {:hours 16}) ;; => {:hours -1}
 (difference {:hours 16} {:hours 17}) ;; => {:hours 1}

 (difference {:hour 17, :utc {:hours 3}}
             {:hour 16, :utc {:hours 2}}) ;; => {:hours 0}


 {:inch 11, :foot 5}
 {:inches 11, :feet 5}

 ;; фактор пространство по классу эквивалентности datetime и построил на этом афинное пространство


 ;1234


 ;[4 -3 2 1]
 ;4 * 10^0
 ;-3 * 10^1
 ;2 * 10^2
 ;1 * 10^3
 ;1174

 ;1000



 (abs {:months -2, :days 4})

 )

