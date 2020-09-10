(ns unit-map.io-test
  (:require [clojure.test :refer :all]
            [matcho.core :as matcho]
            [unit-map.util :as u]
            [unit-map.ops :as ops]
            [unit-map.type.chrono.datetime :as datetime]
            [unit-map.type.chrono.util.misc :as chrono.misc]
            [unit-map.io :as sut]
            [clojure.string :as str]))


(use-fixtures
  :once
  (fn [t]
    (defmethod ops/definition :default-type [_] datetime/gregorian-military)
    (t)))


(deftest nil-safe
  (matcho/match (sut/parse nil nil) {})
  (matcho/match (sut/format nil [:day]) "01")
  (matcho/match (sut/format nil nil) ""))


(deftest parse-format-test
  (testing "parse" ;; TODO: add meta tests
    (testing "numeral representation of month"
      (matcho/match
       (sut/parse "2011-01-01" chrono.misc/iso-fmt)
       {:year 2011 :month 1 :day 1})

      (matcho/match
       (sut/parse "2011-01-01T12:00" chrono.misc/iso-fmt)
       {:year 2011 :month 1 :day 1 :hour 12 :min 0})

      (matcho/match
       (sut/parse "2011-01-01T12:00:00" chrono.misc/iso-fmt)
       {:year 2011 :month 1 :day 1 :hour 12 :min 0 :sec 0})

      (matcho/match
       (sut/parse "2011-01-01T12:04:05.100" chrono.misc/iso-fmt)
       {:year 2011 :month 1 :day 1 :hour 12 :min 4 :sec 5 :ms 100})

      (matcho/match
       (sut/parse "16.09.2019 23:59:01" [:day \. :month \. :year \space :hour \: :min \: :sec])
       {:day 16, :month 9, :year 2019, :hour 23, :min 59, :sec 1})))

  (testing "format"
    (is (= "12/01/2010" (sut/format {:year 2010 :month 12 :day 1} [:month "/" :day "/" :year]))))


  (testing "roundtrip"
    (let [t {:year 2019, :month 9, :day 16, :hour 23, :min 0, :sec 38, :ms 911}]
      (matcho/match (-> t
                        (sut/format chrono.misc/iso-fmt)
                        (sut/parse chrono.misc/iso-fmt))
                    t)))

  (testing "format with specified width"
    (is (= "06.03.20"
           (sut/format {:year 2020 :month 3 :day 6} [:day \. :month \. [:year 2]])))
    (is (= "06.03.2020"
           (sut/format {:year 2020 :month 3 :day 6} [:day \. :month \. :year])))
    (is (= "2020.03.06"
           (sut/format {:year 2020 :month 3 :day 6} [:year \. :month \. :day])))
    (is (= "20.03.06"
           (sut/format {:year 2020 :month 3 :day 6} [[:year 2] \. :month \. :day])))
    (is (= "--1+ baz"
           (sut/format {:foo 1, :bar "baz"} [[:foo 3 \-] \+ [:bar 4]])))
    (testing "custom keys"
      (is (= "5.000001234" (sut/format {:sec 5 :ns 1234} [[:sec 1] \. :ms [:ns 6]])))
      (is (= "5.000123456" (sut/format {:sec 5 :ns 123456} [[:sec 1] \. :ms :ns])))))

  (testing "parse should return parsed value even if format not strictly cosistent"
    (is (= {:year 2011}
           (sut/parse "2011-01-01" [:year "-" :month])))

    (matcho/match (sut/parse "2011-01-01" [:year "-" :month "-" :day "T" :hour])
                  {:year 2011 :month 1}))

  (testing "parsing invalid strings should return only parsed part"
    (matcho/match (sut/parse "2020-12-ab" [:year "-" :month "-" :day])
                  {:year 2020 :month 12})))


(deftest strict-parse-test
  (testing "strict parse should return value when format exact match"
    (matcho/match (sut/parse "2011-01-01" [:year \- :month \- :day] :strict true)
                  {:year 2011 :month 1 :day 1})

    (matcho/match (sut/parse "16.09.2019 23:59:01" [:day \. :month \. :year \space :hour \: :min \: :sec] :strict true)
                  {:day 16, :month 9, :year 2019, :hour 23, :min 59, :sec 1}))

  (testing "strict parse should return nil when format not strictly consistent"
    (matcho/match (sut/parse "2011-01-" [:year "-" :month "-" :day] :strict true)
                  nil)

    (matcho/match
     (sut/parse "2011-01-01" [:year "-" :month "-" :day "T" :hour] :strict true)
     nil))

  (testing "parsing invalid strings should return nil"
    (matcho/match
     (sut/parse "2011!23" [:year "-" :month] :strict true)
     nil)

    (matcho/match
     (sut/parse "2011-12" [:year "-" :month "-" :day] :strict true)
     nil)))


(deftest parse-name-test
  (testing "testing months parsing \n"
    (let [cases {"jan" 1
                 "FEB" 2
                 "March" 3
                 "april" 4
                 "MaY." 5
                 "JUNE" 6}
          test-fn (fn [[inp res]]
                    (testing (str "parsing: " inp)
                      (is (= (sut/parse-name inp :month nil) res))))]
      (doall
       (map test-fn cases)))))


(deftest format-str-test
  (testing "literal representation of month"
    (testing "default lang"
      (matcho/match
       (sut/parse "16 Jan 2019 23:59:01"
                  [:day \space :month \space :year \space :hour \: :min \: :sec])
       {:day 16, :month 1, :year 2019, :hour 23, :min 59, :sec 1})

      (matcho/match
       (sut/parse "31 December 2023 13:30:19"
                  [:day \space :month \space :year \space :hour \: :min \: :sec])
       {:day 31, :month 12, :year 2023, :hour 13, :min 30, :sec 19})
      (matcho/match
       (sut/parse "31 march 2023 13:30:19"
                  [:day \space :month \space :year \space :hour \: :min \: :sec])
       {:day 31, :month 3, :year 2023, :hour 13, :min 30, :sec 19})
      (matcho/match
       (sut/parse "28 FEB 2023 13:30:19"
                  [:day \space :month \space :year \space :hour \: :min \: :sec])
       {:day 28, :month 2, :year 2023, :hour 13, :min 30, :sec 19})
      (matcho/match
       (sut/parse "jun. 28 9999" [:month \space :day \space :year])
       {:day 28, :month 6, :year 9999}))

    (testing "en"
      (matcho/match
       (sut/parse "sep. 19 2023" ^:en[:month \space :day \space :year])
       {:day 19, :month 9, :year 2023}))

    (testing "ru"
      (matcho/match
       (sut/parse "19 января" [:day \space ^:ru[:month]])
       {:day 19 :month 1})
      (matcho/match
       (sut/parse "февраль 19" [^:ru[:month] \space :year])
       {:year 19 :month 2})
      (matcho/match
       (sut/parse "окт 9:36" [^:ru[:month] \space :hour \: :min])
       {:month 10 :hour 9 :min 36}))

    (testing "invalid month name"
      (matcho/match (sut/parse "19 month" [:year \space ^:ru[:month]])
                    {:year 19})
      (matcho/match (sut/parse "month 19" [^:ru[:month] \space :year])
                    {:year 19, :month nil}))

    (testing "invalid locale name"
      (matcho/match (sut/parse "январь 19" [^:en[:month] \space :year]) {:year 19, :month nil})))

  (testing "month-name"
    (is (= "Октябрь 2009"
           (sut/format {:year 2009 :month 10} [^:ru[:month] \space :year])))
    (is (= "Sep. 1"
           (sut/format {:month 9 :day 1} [^:en[:month :short] \. \space [:day 1]])))
    (is (= "1:month:entest"
           (sut/format {:month 1}
                       [^:en[:month (fn [v {:keys [value lang pad-str]}] (str (get v value) value lang pad-str)) "test"]])))
    (is (= "2"
           (sut/format {:month 9 :day 1} [[:month 0 (fn [& args] (count args))]]))))

  (testing "month names"
    (is (= "November" (sut/format-el {:month 11} nil ^:en[:month])))
    (is (= "Март" (sut/format-el {:month 3} nil ^:ru[:month])))
    (is (= "Aug" (sut/format-el {:month 8} nil ^:en[:month :short])))
    (is (= "09" (sut/format-el {:month 9} nil [:month :short]))))

  (testing "strict parsing month names"
    (matcho/match
     (sut/parse "2011-JUL" [:year "-" :month] :strict true) {:year 2011 :month 7})))
