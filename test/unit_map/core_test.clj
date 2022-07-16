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

  (sut/regseq! treg_ :ms   #unit-map/seq[0 1 .. 999] :next :sec)
  (sut/regseq! treg_ :sec  #unit-map/seq[0 1 .. 59] :next :min)
  (sut/regseq! treg_ :min  #unit-map/seq[0 1 .. 59] :next :hour)
  (sut/regseq! treg_ :hour #unit-map/seq[0 1 .. 23] :next :day)

  (sut/regseq! treg_ :day   #unit-map/seq[1 2 .. days-in-month] :next :month)
  (sut/regseq! treg_ :month #unit-map/seq[:jan :feb  :mar :apr :may  :jun :jul :aug  :sep :oct :nov  :dec] :next :year)
  (sut/regseq! treg_ :year  #unit-map/seq[##-Inf .. -2 -1 1 2 .. ##Inf])

  (sut/regseq! treg_ :ms   #unit-map/seq[0 1 .. ##Inf])
  (sut/regseq! treg_ :sec  #unit-map/seq[0 1 .. ##Inf])
  (sut/regseq! treg_ :hour #unit-map/seq[0 1 .. ##Inf])
  (sut/regseq! treg_ :day  #unit-map/seq[0 1 .. ##Inf]) #_"NOTE: should start with 0 or with 1?"

  (sut/regsys! treg_ 'timestamp  [:ms])
  (sut/regsys! treg_ 'ms-hour    [:ms :sec :min :hour])
  (sut/regsys! treg_ 'ms-day     [:ms :sec :min :hour :day])
  (sut/regsys! treg_ 'ms-year    [:ms :sec :min :hour :day :month :year])
  (sut/regsys! treg_ 'month-year [:month :year])
  (sut/regsys! treg_ 'date       [:day :month :year])

  (->> (for [[sys sys-def] (:systems @treg_)
             :when (symbol? sys)]
         (list 'def sys sys-def))
       (cons 'do)
       eval #_"TODO: refactor this"))


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


(t/deftest parse&format-test
  (def iso-fmt [:year "-" :month "-" :day "T" :hour ":" :min ":" :sec "." :ms])

  (t/testing "nil-safe"
    (t/is (= {} (sut/parse nil nil)))
    #_(t/is (= "01" (sut/format nil [:day])))
    (t/is (= "" (sut/format nil nil))))


  (t/testing "parse"
    (t/testing "numeral representation of month"
      (t/is (= {:year 2011 :month 1 :day 1}
               (sut/parse "2011-01-01" iso-fmt)))

      (t/is (= {:year 2011 :month 1 :day 1 :hour 12 :min 0}
               (sut/parse "2011-01-01T12:00" iso-fmt)))

      (t/is (= {:year 2011 :month 1 :day 1 :hour 12 :min 0 :sec 0}
               (sut/parse "2011-01-01T12:00:00" iso-fmt)))

      (t/is (= {:year 2011 :month 1 :day 1 :hour 12 :min 4 :sec 5 :ms 100}
               (sut/parse "2011-01-01T12:04:05.100" iso-fmt)))

      (t/is (= {:day 16, :month 9, :year 2019, :hour 23, :min 59, :sec 1}
               (sut/parse "16.09.2019 23:59:01" [:day \. :month \. :year \space :hour \: :min \: :sec])))))

  (t/testing "format"
    (t/is (= "12/01/2010" (sut/format {:year 2010 :month 12 :day 1} [:month "/" :day "/" :year]))))

  (t/testing "roundtrip"
    (let [t {:year 2019, :month 9, :day 16, :hour 23, :min 0, :sec 38, :ms 911}]
      (t/is (= t
               (-> t
                   (sut/format iso-fmt)
                   (sut/parse iso-fmt))))))

  (t/testing "format with specified width"
    (t/is (= "06.03.20"
           (sut/format {:year 2020 :month 3 :day 6} [:day \. :month \. [:year 2]])))
    (t/is (= "06.03.2020"
           (sut/format {:year 2020 :month 3 :day 6} [:day \. :month \. :year])))
    (t/is (= "2020.03.06"
           (sut/format {:year 2020 :month 3 :day 6} [:year \. :month \. :day])))
    (t/is (= "20.03.06"
           (sut/format {:year 2020 :month 3 :day 6} [[:year 2] \. :month \. :day])))
    (t/is (= "--1+ baz"
           (sut/format {:foo 1, :bar "baz"} [[:foo 3 \-] \+ [:bar 4]]))))

  (t/testing "parse should return parsed value even if format not strictly cosistent"
    (t/is (= {:year 2011}
           (sut/parse "2011-01-01" [:year "-" :month])))

    (t/is (= {:year 2011 :month 1 :day 1}
             (sut/parse "2011-01-01" [:year "-" :month "-" :day "T" :hour]))))

  (t/testing "parsing invalid strings should return only parsed part"
    (t/is (= {:year 2020 :month 12}
             (sut/parse "2020-12-ab" [:year "-" :month "-" :day]))))


  (t/testing "strict-parse"
    (t/testing "strict parse should return value when format exact match"
      (t/is (= {:year 2011 :month 1 :day 1}
               (sut/parse "2011-01-01" [:year \- :month \- :day] :strict true)))

      (t/is (= {:day 16, :month 9, :year 2019, :hour 23, :min 59, :sec 1}
               (sut/parse "16.09.2019 23:59:01" [:day \. :month \. :year \space :hour \: :min \: :sec] :strict true))))

    (t/testing "strict parse should return nil when format not strictly consistent"
      (t/is (= nil
               (sut/parse "2011-01-" [:year "-" :month "-" :day] :strict true)))

      (t/is (=
              nil
              (sut/parse "2011-01-01" [:year "-" :month "-" :day "T" :hour] :strict true))))

    (t/testing "parsing invalid strings should return nil"
      (t/is (=
              nil
              (sut/parse "2011!23" [:year "-" :month] :strict true)))

      (t/is (=
              nil
              (sut/parse "2011-12" [:year "-" :month "-" :day] :strict true)))))


  (t/testing "format-str"
    (t/testing "literal representation of month"
      (t/testing "default lang"
        (t/is (=
                {:day 16, :month 1, :year 2019, :hour 23, :min 59, :sec 1}
                (sut/parse "16 Jan 2019 23:59:01"
                           [:day \space :month \space :year \space :hour \: :min \: :sec])))

        (t/is (=
                {:day 31, :month 12, :year 2023, :hour 13, :min 30, :sec 19}
                (sut/parse "31 December 2023 13:30:19"
                           [:day \space :month \space :year \space :hour \: :min \: :sec])))
        (t/is (=
                {:day 31, :month 3, :year 2023, :hour 13, :min 30, :sec 19}
                (sut/parse "31 march 2023 13:30:19"
                           [:day \space :month \space :year \space :hour \: :min \: :sec])))
        (t/is (=
                {:day 28, :month 2, :year 2023, :hour 13, :min 30, :sec 19}
                (sut/parse "28 FEB 2023 13:30:19"
                           [:day \space :month \space :year \space :hour \: :min \: :sec])))
        (t/is (=
                {:day 28, :month 6, :year 9999}
                (sut/parse "jun. 28 9999" [:month \space :day \space :year]))))

      (t/testing "en"
        (t/is (=
                {:day 19, :month 9, :year 2023}
                (sut/parse "sep. 19 2023" ^:en[:month \space :day \space :year]))))

      (t/testing "ru"
        (t/is (=
                {:day 19 :month 1}
                (sut/parse "19 января" [:day \space ^:ru[:month]])))
        (t/is (=
                {:year 19 :month 2}
                (sut/parse "февраль 19" [^:ru[:month] \space :year])))
        (t/is (=
                {:month 10 :hour 9 :min 36}
                (sut/parse "окт 9:36" [^:ru[:month] \space :hour \: :min]))))

      (t/testing "invalid month name"
        (t/is (= {:year 19}
                 (sut/parse "19 month" [:year \space ^:ru[:month]])))
        (t/is (= {:year 19}
                 (sut/parse "month 19" [^:ru[:month] \space :year]))))

      (t/testing "invalid locale name"
        (t/is (= {:year 19} (sut/parse "январь 19" [^:en[:month] \space :year])))))

    (t/testing "month-name"
      (t/is (= "Октябрь 2009"
               (sut/format {:year 2009 :month 10} [^:ru[:month] \space :year])))
      (t/is (= "Sep. 1"
               (sut/format {:month 9 :day 1} [^:en[:month :short] \. \space [:day 1]])))
      (t/is (= "1:month:entest"
               (sut/format {:month 1}
                           [^:en[:month (fn [v {:keys [value lang pad-str]}] (str (get v value) value lang pad-str)) "test"]])))
      (t/is (= "2"
               (sut/format {:month 9 :day 1} [[:month 0 (fn [& args] (count args))]]))))

    (t/testing "strict parsing month names"
      (t/is (=
              {:year 2011 :month 7} (sut/parse "2011-JUL" [:year "-" :month] :strict true))))

    (t/testing "parsing formatting without separators"
      (t/testing "fmt vec with regex"
        (let [fmt [[:year #"\d\d" 2] [:month #"\d\d"] [:day #"\d\d"] [:hour #"\d\d"] [:min #"\d\d"]]]
          (t/is (= {:year 21 :month 3 :day 2 :hour 8 :min 5}
                   (sut/parse "2103020805" fmt)))

          (t/is (= "2103020805" (sut/format {:year 21 :month 3 :day 2 :hour 8 :min 5} fmt)))))

      (t/testing "fmt vec with width"
        (let [fmt [[:year 2] [:month 2] [:day 2] [:hour 2] [:min 2]]]
          (t/is (= {:year 21 :month 3 :day 2 :hour 8 :min 5}
                   (sut/parse "2103020805" fmt)))

          (t/is (= "2103020805" (sut/format {:year 21 :month 3 :day 2 :hour 8 :min 5} fmt))))))))


(t/deftest ^:kaocha/pending demo-test
  (sut/regseq! treg_ :ms   #unit-map/seq[0 1 .. 999] :next :sec)
  (sut/regseq! treg_ :sec  #unit-map/seq[0 1 .. 59] :next :min)
  (sut/regseq! treg_ :min  #unit-map/seq[0 1 .. 59] :next :hour)
  (sut/regseq! treg_ :hour #unit-map/seq[0 1 .. 23] :next :day)

  (sut/regseq! treg_ :day   #unit-map/seq[1 2 .. days-in-month] :next :month)
  (sut/regseq! treg_ :month #unit-map/seq[:jan :feb  :mar :apr :may  :jun :jul :aug  :sep :oct :nov  :dec] :next :year)
  (sut/regseq! treg_ :year  #unit-map/seq[##-Inf .. -2 -1 1 2 .. ##Inf])

  (sut/regsys! treg_ 'ms-year    [:ms :sec :min :hour :day :month :year])

  #_(sut/deffmt :iso/month [:month (fn [v fmt-el] '???)])
  #_(sut/deffmt :iso/day [:day 2 "0"])

  #_"NOTE: arithmetics for now can be stubbed with simple update/inc etc"
  #_"NOTE: need some configs to map months enum to numbers"
  #_"NOTE: for useqs consisting of only static ranges calculate leading 0 padding automatically"

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
