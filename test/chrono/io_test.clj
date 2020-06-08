(ns chrono.io-test
  (:require [clojure.test :refer :all]
            [matcho.core :as matcho]
            [chrono.io :as sut]
            [chrono.datetime :as cd]
            [chrono.interval :as ci]
            [chrono.locale-ru]
            [clojure.string :as str]))

(deftest parse-format-test
  (testing "nil safe"
    (matcho/match (sut/parse nil) nil)
    (matcho/match (sut/format nil [::cd/day]) "00"))

  (testing "parse"
    (testing "numeral representation of month"
      (matcho/match
       (sut/parse "2011-01-01")
       #::cd{:year 2011 :month 1 :day 1})

      (matcho/match
       (sut/parse "2011-01-01T12:00")
       #::cd{:year 2011 :month 1 :day 1 :hour 12 :min 0})

      (matcho/match
       (sut/parse "2011-01-01T12:00:00")
       #::cd{:year 2011 :month 1 :day 1 :hour 12 :min 0 :sec 0})

      (matcho/match
       (sut/parse "2011-01-01T12:04:05.100")
       #::cd{:year 2011 :month 1 :day 1 :hour 12 :min 4 :sec 5 :ms 100})

      (matcho/match
       (sut/parse "16.09.2019 23:59:01" [::cd/day \. ::cd/month \. ::cd/year \space
                                         ::cd/hour \: ::cd/min \: ::cd/sec])
       #::cd{:day 16, :month 9, :year 2019, :hour 23, :min 59, :sec 1}))

    (testing "literal representation of month"
      (testing "default lang"
        (matcho/match
         (sut/parse "16 Jan 2019 23:59:01"
                    [::cd/day \space ::cd/month \space ::cd/year \space
                     ::cd/hour \: ::cd/min \: ::cd/sec])
         #::cd{:day 16, :month 1, :year 2019, :hour 23, :min 59, :sec 1})

        (matcho/match
         (sut/parse "31 December 2023 13:30:19"
                    [::cd/day \space ::cd/month \space ::cd/year \space
                     ::cd/hour \: ::cd/min \: ::cd/sec])
         #::cd{:day 31, :month 12, :year 2023, :hour 13, :min 30, :sec 19})
        (matcho/match
         (sut/parse "31 march 2023 13:30:19"
                    [::cd/day \space ::cd/month \space ::cd/year \space
                     ::cd/hour \: ::cd/min \: ::cd/sec])
         #::cd{:day 31, :month 3, :year 2023, :hour 13, :min 30, :sec 19})
        (matcho/match
         (sut/parse "28 FEB 2023 13:30:19"
                    [::cd/day \space ::cd/month \space ::cd/year \space
                     ::cd/hour \: ::cd/min \: ::cd/sec])
         #::cd{:day 28, :month 2, :year 2023, :hour 13, :min 30, :sec 19})
        (matcho/match
         (sut/parse "jun. 28 9999" [::cd/month \space ::cd/day \space ::cd/year])
         #::cd{:day 28, :month 6, :year 9999}))
      (testing "en"
        (matcho/match
         (sut/parse "sep. 19 2023" ^:en[::cd/month \space ::cd/day \space ::cd/year])
         #::cd{:day 19, :month 9, :year 2023}))
      (testing "ru"
        (matcho/match
         (sut/parse "19 января" ^:ru[::cd/day \space ::cd/month])
         #::cd{:day 19 :month 1})
        (matcho/match
         (sut/parse "февраль 19" ^:ru[::cd/month \space ::cd/year])
         #::cd{:year 19 :month 2})
        (matcho/match
         (sut/parse "окт 9:36" ^:ru[::cd/month \space ::cd/hour \: ::cd/min])
         #::cd{:month 10 :hour 9 :min 36}))
      (testing "invalid month name"
        (matcho/match (sut/parse "19 month" ^:ru[::cd/year \space ::cd/month]) {::cd/year 19})
        (matcho/match (sut/parse "month 19" ^:ru[::cd/month \space ::cd/year]) nil))

      (testing "invalid locale name"
        (matcho/match (sut/parse "январь 19" ^:en[::cd/month \space ::cd/year]) nil)))

    (testing "intervals"
      (is (= #::ci{:hour 20 :min 30}
             (sut/parse "20:30" [::ci/hour \: ::ci/min])))
      (is (= #::ci{:day 50}
             (sut/parse "50d" [::ci/day \d])))))

  (testing "format"
    (is (= "12/01/2010"
           (sut/format #::cd{:year 2010 :month 12 :day 1}
                       [::cd/month "/" ::cd/day "/" ::cd/year])))
    (testing "month-name"
      (is (= "Октябрь 2009"
             (sut/format #::cd{:year 2009 :month 10}
                         ^:ru[::cd/month \space ::cd/year])))
      (is (= "Sep. 1"
             (sut/format #::cd{:month 9 :day 1}
                         ^:en[[::cd/month :short] \. \space [::cd/day 1]])))
      (is (= "1:month:entest"
             (sut/format {::cd/month 1}
                         ^:en[[::cd/month
                               (fn [v [kw _ s] lc]
                                 (str/join [v (-> kw name keyword) lc s]))
                               "test"]])))
      (is (= "3"
             (sut/format #::cd{:month 9 :day 1}
                         [[::cd/month (fn [& args] (count args))]]))))
    (testing "custom keys"
      (is (= "5.000001234" (sut/format {::cd/sec 5 :ns 1234}
                                       [[::cd/sec 1] \. ::cd/ms [:ns 6]])))
      (is (= "5.000123456" (sut/format {::cd/sec 5 :ns 123456}
                                       [[::cd/sec 1] \. ::cd/ms :ns]))))
    (testing "intervals"
      (is (= "20 hours 3 minutes"
             (sut/format #::ci{:hour 20 :min 3}
                         [::ci/hour " hours " ::ci/min " minutes"])))
      (is (= "50 days"
             (sut/format #::ci{:day 50}
                         [::ci/day " days"])))))

  (testing "roundtrip"
    (let [t #::cd{:year 2019, :month 9, :day 16, :hour 23, :min 0, :sec 38, :ms 911}]
      (matcho/match (sut/parse (sut/format t)) t))
    (let [i #::ci{:day 50 :hour 20 :min 30 :sec 5 :ms 123}
          fmt [::ci/day \space ::ci/hour \space ::ci/min \space ::ci/sec \. ::ci/ms]]
      (is (= i (-> i (sut/format fmt) (sut/parse fmt))))))

  (testing "format with specified width"
    (is (= "06.03.20"
           (sut/format #::cd{:year 2020 :month 3 :day 6}
                       [::cd/day \. ::cd/month \. [::cd/year 2]])))
    (is (= "06.03.2020"
           (sut/format #::cd{:year 2020 :month 3 :day 6}
                       [::cd/day \. ::cd/month \. ::cd/year])))
    (is (= "2020.03.06"
           (sut/format #::cd{:year 2020 :month 3 :day 6}
                       [::cd/year \. ::cd/month \. ::cd/day])))
    (is (= "20.03.06"
           (sut/format #::cd{:year 2020 :month 3 :day 6}
                       [[::cd/year 2] \. ::cd/month \. ::cd/day]))))
  (testing "parse should return parsed value even if format not strictly cosistent"
    (matcho/match
     (sut/parse "2011-01-01" [::cd/year "-" ::cd/month])
     #::cd{:year 2011 :month 1})

    (matcho/match
     (sut/parse "2011-01-01" [::cd/year "-" ::cd/month "-" ::cd/day "T" ::cd/hour])
     #::cd{:year 2011 :month 1})))

(testing "parsing invalid strings should return only parsed part"
  (matcho/match
   (sut/parse "2020-12-ab" [::cd/year "-" ::cd/month "-" ::cd/day])
   #::cd{:year 2020 :month 12}))

(deftest strict-parse-test
  (testing "strict parse should return value when format exact match"
    (matcho/match
     (sut/strict-parse "2011-01-01" [::cd/year \- ::cd/month \- ::cd/day])
     #::cd{:year 2011 :month 1 :day 1})

    (matcho/match
     (sut/strict-parse "16.09.2019 23:59:01" [::cd/day \. ::cd/month \. ::cd/year \space
                                              ::cd/hour \: ::cd/min \: ::cd/sec])
     #::cd{:day 16, :month 9, :year 2019, :hour 23, :min 59, :sec 1}))

  (testing "strict parse should return nil when format not strictly consistent"
    (matcho/match
     (sut/strict-parse "2011-01-01" [::cd/year "-" ::cd/month])
     nil)

    (matcho/match
     (sut/strict-parse "2011-01-01" [::cd/year "-" ::cd/month "-" ::cd/day "T" ::cd/hour])
     nil))

  (testing "parsing invalid strings should return nil"
    (matcho/match
     (sut/strict-parse "2011-23" [::cd/year "-" ::cd/month]) nil)

    (matcho/match
     (sut/strict-parse "2011-12" [::cd/month "-" ::cd/year]) nil))

  (testing "strict parsing month names"
    (matcho/match
     (sut/strict-parse "2011-JUL" [::cd/year "-" ::cd/month])
     #::cd{:year 2011 :month 7})))

(deftest format-str-test
  (testing "month names"
    (is (= "November" (#'sut/format-str 11 [::cd/month] :en)))
    (is (= "Март" (#'sut/format-str 3 [::cd/month] :ru)))
    (is (= "Aug" (#'sut/format-str 8 [::cd/month :short] :en)))
    (is (= "09" (#'sut/format-str 9 [::cd/month :short] nil)))))

(deftest from-epoch-test
  (is (= #::cd{:day 22, :month 4, :year 2020, :sec 40, :min 49, :hour 1}
         (sut/from-epoch 1587520180)))
  (is (= #::cd{:day 1, :month 1, :year 1970}
         (sut/from-epoch 0))))

(deftest to-epoch-test
  (is (= 1587520180
         (sut/to-epoch #::cd{:day 22, :month 4, :year 2020, :sec 40, :min 49, :hour 1})))
  (is (= 0
         (sut/to-epoch #::cd{:day 1, :month 1, :year 1970, :sec 0, :min 0, :hour 0}))))
