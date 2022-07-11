(ns unit-map.impl.ops-test
  (:require [unit-map.impl.ops :as sut]
            [unit-map.core :as umap]
            [clojure.test :as t]))


(def registry-atom (atom nil))


(do
  (defn leap-year? [{::keys [year]}]
    (and (zero? (rem year 4))
         (or (pos? (rem year 100))
             (zero? (rem year 400)))))


  (defn days-in-month [{:as date, ::keys [month]}]
    (condp contains? month
      #{:jan :mar :may :jul :aug :oct :dec} 31
      #{:apr :jun :sep :nov}                30
      #{:feb}                               (if (leap-year? date) 29 28)
      ##Inf))

  (umap/defseq registry-atom ::hour   #unit-map/seq[0 1 .. 23 -> ::day])

  (umap/defseq registry-atom ::ampm-hour   #unit-map/seq[::hour <=> 12 1 2 .. 11 -> ::ampm-period])
  (umap/defseq registry-atom ::ampm-period #unit-map/seq[:am :pm])

  (umap/defseq registry-atom ::day   #unit-map/seq[1 2 .. days-in-month -> ::month])
  (umap/defseq registry-atom ::month #unit-map/seq[:jan :feb  :mar :apr :may  :jun :jul :aug  :sep :oct :nov  :dec -> ::year])
  (umap/defseq registry-atom ::year  #unit-map/seq[##-Inf .. -2 -1 1 2 .. ##Inf])

  (umap/defseq registry-atom ::epoch-year  #unit-map/seq[::year <=>
                                                        (fn [{::keys [epoch]}]
                                                          (if (= :BC epoch) ##Inf 1))
                                                        (fn [{::keys [epoch]}]
                                                          (if (= :BC epoch) -1 1))
                                                        ..
                                                        (fn [{::keys [epoch]}]
                                                          (if (= :BC epoch) 1 ##Inf))
                                                        -> ::epoch])

  (umap/defseq registry-atom ::epoch  #unit-map/seq[:BC :AD])

  (umap/defsys registry-atom 'datetime   [::hour ::day ::month ::year])
  (umap/defsys registry-atom 'date       [::day ::month ::year])
  (umap/defsys registry-atom 'month-year [::month ::year])
  (umap/defsys registry-atom 'year-epoch [::epoch-year ::epoch])
  (umap/defsys registry-atom 'am-pm-time [::ampm-hour ::ampm-period])

  (->> (for [[sys sys-def] (:systems @registry-atom)
             :when (symbol? sys)]
         (list 'def sys sys-def))
       (cons 'do)
       eval #_"TODO: refactor this"))


(t/deftest arithmetic-test
  (t/testing "inc-unit"
    (t/testing "am-pm clock"
      (def value {::ampm-hour 12, ::ampm-period :am})

      (t/is (= [{::ampm-hour 12, ::ampm-period :am} {::ampm-hour 1, ::ampm-period :am} {::ampm-hour 2, ::ampm-period :am}
                {::ampm-hour 3, ::ampm-period :am} {::ampm-hour 4, ::ampm-period :am} {::ampm-hour 5, ::ampm-period :am}
                {::ampm-hour 6, ::ampm-period :am} {::ampm-hour 7, ::ampm-period :am} {::ampm-hour 8, ::ampm-period :am}
                {::ampm-hour 9, ::ampm-period :am} {::ampm-hour 10, ::ampm-period :am} {::ampm-hour 11, ::ampm-period :am}

                {::ampm-hour 12, ::ampm-period :pm} {::ampm-hour 1, ::ampm-period :pm} {::ampm-hour 2, ::ampm-period :pm}
                {::ampm-hour 3, ::ampm-period :pm} {::ampm-hour 4, ::ampm-period :pm} {::ampm-hour 5, ::ampm-period :pm}
                {::ampm-hour 6, ::ampm-period :pm} {::ampm-hour 7, ::ampm-period :pm} {::ampm-hour 8, ::ampm-period :pm}
                {::ampm-hour 9, ::ampm-period :pm} {::ampm-hour 10, ::ampm-period :pm} {::ampm-hour 11, ::ampm-period :pm}]
               (take 24 (iterate (partial sut/inc-unit @registry-atom ::ampm-hour) value)))))

    (t/testing "epoch"
      (t/is (= [{::year -2} {::year -1} {::year 1} {::year 2}]
               (take 4 (iterate #(sut/inc-unit @registry-atom ::year %) {::year -2}))))

      (t/is (= [{::epoch :BC, ::epoch-year 2} {::epoch :BC, ::epoch-year 1}
                {::epoch :AD, ::epoch-year 1} {::epoch :AD, ::epoch-year 2}]
               (take 4 (iterate #(sut/inc-unit @registry-atom ::epoch-year %) {::epoch :BC ::epoch-year 2})))))

    (t/testing "calendar"
      (def value {::day 1, ::month :jan, ::year 2020})

      (def calendar (->> value
                         (iterate (partial sut/inc-unit @registry-atom ::day))
                         (take-while (comp #{2020} ::year))
                         (partition-by ::month)))

      (t/is (= 12 (count calendar)))
      (t/is (= 366 (count (flatten calendar))))))

  (t/testing "dec-unit"
    (t/testing "am-pm clock"
      (def value {::ampm-hour 11, ::ampm-period :pm})

      (t/is (= [{::ampm-hour 11, ::ampm-period :pm} {::ampm-hour 10, ::ampm-period :pm} {::ampm-hour 9, ::ampm-period :pm}
                {::ampm-hour 8, ::ampm-period :pm} {::ampm-hour 7, ::ampm-period :pm} {::ampm-hour 6, ::ampm-period :pm}
                {::ampm-hour 5, ::ampm-period :pm} {::ampm-hour 4, ::ampm-period :pm} {::ampm-hour 3, ::ampm-period :pm}
                {::ampm-hour 2, ::ampm-period :pm} {::ampm-hour 1, ::ampm-period :pm} {::ampm-hour 12, ::ampm-period :pm}

                {::ampm-hour 11, ::ampm-period :am} {::ampm-hour 10, ::ampm-period :am} {::ampm-hour 9, ::ampm-period :am}
                {::ampm-hour 8, ::ampm-period :am} {::ampm-hour 7, ::ampm-period :am} {::ampm-hour 6, ::ampm-period :am}
                {::ampm-hour 5, ::ampm-period :am} {::ampm-hour 4, ::ampm-period :am} {::ampm-hour 3, ::ampm-period :am}
                {::ampm-hour 2, ::ampm-period :am} {::ampm-hour 1, ::ampm-period :am} {::ampm-hour 12, ::ampm-period :am}]
               (take 24 (iterate (partial sut/dec-unit @registry-atom ::ampm-hour) value)))))

    (t/testing "calendar"
      (def value {::day 31, ::month :dec, ::year 2019})

      (def calendar (->> value
                         (iterate (partial sut/dec-unit @registry-atom ::day))
                         (take-while (comp #{2019} ::year))
                         (partition-by ::month)))

      (t/is (= 12 (count calendar)))
      (t/is (= 365 (count (flatten calendar))))))

  (t/testing "add-to-unit"
    (t/is (= {::hour 0, ::day 22, ::month :aug, ::year 2044}
             (sut/add-to-unit @registry-atom {::hour 0 ::day 1, ::month :jan, ::year 2020} ::hour 216000)))
    (t/is (= {::hour 0 ,::year 1995, ::month :may, ::day 12}
             (sut/add-to-unit @registry-atom {::hour 0 ::day 1, ::month :jan, ::year 2020} ::hour -216000)))
    (t/is (= {::day 1, ::month :jan, ::year 2020}
             (sut/add-to-unit @registry-atom {::day 1, ::month :jan, ::year 2020} ::hour 0))))

  (t/testing "subtract-from-unit"
    (t/is (= {::hour 0, ::day 22, ::month :aug, ::year 2044}
             (sut/subtract-from-unit @registry-atom {::day 1, ::month :jan, ::year 2020} ::hour -216000)))
    (t/is (= {::hour 0,::year 1995, ::month :may, ::day 12}
             (sut/subtract-from-unit @registry-atom {::day 1, ::month :jan, ::year 2020} ::hour 216000)))
    (t/is (= {::day 1, ::month :jan, ::year 2020}
             (sut/subtract-from-unit @registry-atom {::day 1, ::month :jan, ::year 2020} ::hour 0)))))

