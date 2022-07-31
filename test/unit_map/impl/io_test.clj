(ns unit-map.impl.io-test
  (:require [unit-map.impl.io :as sut]
            [clojure.test :as t]))


#_(t/deftest name-test
  (t/testing "testing months parsing"
    (let [cases {"jan" 1
                 "FEB" 2
                 "March" 3
                 "april" 4
                 "MaY." 5
                 "JUNE" 6}
          test-fn (fn [[inp res]]
                    (t/testing (str "parsing: " inp)
                      (t/is (= (sut/parse-name inp :month nil) res))))]
      (doall
        (map test-fn cases))))

  (t/testing "month names formatting"
    (t/is (= "November" (sut/format-el {:month 11} nil ^:en[:month])))
    (t/is (= "Март" (sut/format-el {:month 3} nil ^:ru[:month])))
    (t/is (= "Aug" (sut/format-el {:month 8} nil ^:en[:month :short])))
    (t/is (= "09" (sut/format-el {:month 9} nil [:month :short])))))


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
    #_(t/testing "literal representation of month"
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
        (t/is (= {:year 19} (sut/parse "январь 19" [^:en[:month] \space :year]))))

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
                {:year 2011 :month 7} (sut/parse "2011-JUL" [:year "-" :month] :strict true)))))

    (t/testing "parsing formatting without separators"
      (t/testing "fmt vec with width"
        (def fmt [[:year 2 \0] [:month 2 \0] [:day 2 \0] [:hour 2 \0] [:min 2 \0]])

        (t/is (= {:year 21 :month 3 :day 2 :hour 8 :min 5}
                 (sut/parse "2103020805" fmt)))

        (t/is (= "2103020805" (sut/format {:year 21 :month 3 :day 2 :hour 8 :min 5} fmt)))

        (t/testing "default padding"
          (def fmt [[:year 2] [:month 2] [:day 2] [:hour 2] [:min 2]])

          (t/is (= {:year 21 :month 3 :day 2 :hour 8 :min 5}
                   (sut/parse "21 3 2 8 5" fmt)))

          (t/is (= "21 3 2 8 5" (sut/format {:year 21 :month 3 :day 2 :hour 8 :min 5} fmt)))))))


  (t/testing "map fmt el"
    (def s "200101")
    (def fmt [{:value :year, :width 2, :pad "0"}
              {:value :month , :width 2 :pad 0}
              {:value :day, :width 2 :pad \0}])

    (t/is (= s
             (-> s
                 (sut/parse fmt)
                 (sut/format fmt))))))


(require '[unit-map.impl.system :as system]
         '[unit-map.impl.registry :as registry]
         '[unit-map.impl.util :as util]
         '[unit-map.core]
         '[clojure.string :as str])


(declare format-unit)


(defn format-args [registry format-params umap]
  (cond
    (:unit format-params)
    (get umap (:unit format-params))

    (:element format-params)
    (format-unit registry (:element format-params) umap)

    (:units format-params)
    (->> (:units format-params)
         (into {}
               (keep (fn [unit]
                       (when-let [v (get umap unit)]
                         [unit v]))))
         not-empty)

    (:elements format-params)
    (->> (:elements format-params)
         (into {} (keep (fn [fmt-key]
                          (when-let [v (format-unit registry fmt-key umap)]
                            [fmt-key v]))))
         not-empty)))


(defn format-unit [registry format-key umap]
  (when (seq umap)
    (let [params    (get-in registry [::format format-key])
          arg       (format-args registry params umap)
          format-fn (:format params)
          width     (:width params)
          pad       (:pad params " ")]
      (when (some? arg)
        (cond->> arg
          format-fn (format-fn registry)
          :always   str
          width     (util/pad-str pad width))))))


(declare parse-unit)


(defn parse-res [registry parse-params parsed]
  (cond
    (:unit parse-params)
    {(:unit parse-params) parsed}

    (:units parse-params)
    (into {}
          (keep (fn [u] (when-let [v (get parsed u)]
                          [u v])))
          (:units parse-params))

    (:element parse-params)
    (parse-unit registry (:element parse-params) parsed)

    (:elements parse-params)
    (into {}
          (map #(parse-unit registry % (get parsed %)))
          (:elements parse-params))))


(defn parse-unit [registry format-key value-s]
  (when (not (str/blank? value-s))
    (let [params (get-in registry [::format format-key])
          parsed (if-let [parse-fn (:parse params)]
                   (parse-fn registry value-s)
                   value-s)]
      (some->> parsed
               (parse-res registry params)
               not-empty))))


(defn reg-format! [registry-atom format-name format-params]
  (swap! registry-atom assoc-in [::format format-name] format-params))


(t/deftest parse-format-cfg
  ;; 2015
  ;; 2015-04
  ;; 2015-04-25
  ;; 04/25/2015
  ;; 25 Апр 2015
  ;; 25 07 (апр.) 2015
  ;; Fri Apr 25 2015 12:48:59 GMT+0200 (Eastern European Standard Time)
  ;; 2015-06-30T23:59:60Z
  ;; Roman year

  (do #_"NOTE: fixtures"

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

      (def ns->sec     {:unit :ns,    :next-unit :sec,   :useq #unit-map/useq[0 1 .. 999999999]})
      (def ms->sec     {:unit :ms,    :next-unit :sec,   :useq #unit-map/useq[0 1 .. 999]})
      (def sec->min    {:unit :sec,   :next-unit :min,   :useq #unit-map/useq[0 1 .. 59]})
      (def min->hour   {:unit :min,   :next-unit :hour,  :useq #unit-map/useq[0 1 .. 59]})
      (def hour->day   {:unit :hour,  :next-unit :day,   :useq #unit-map/useq[0 1 .. 23]})
      (def day->month  {:unit :day,   :next-unit :month, :useq #unit-map/useq[0 1 .. days-in-month]})
      (def month->year {:unit :month, :next-unit :year,  :useq #unit-map/useq[:jan :feb  :mar :apr :may  :jun :jul :aug  :sep :oct :nov  :dec]})
      (def years       {:unit :year,                     :useq #unit-map/useq[##-Inf .. -2 -1 1 2 .. ##Inf]})

      (def date [:day :month :year])
      (def datetime [:ms :sec :min :hour :day :month :year])

      (def reg_ (unit-map.core/new-registry))

      (unit-map.core/reg-useqs! reg_ [ns->sec ms->sec sec->min min->hour hour->day day->month month->year years])
      (unit-map.core/reg-systems! reg_ [date datetime]))

  (def base-formats
    {:day {:unit  :day
           :parse (fn [_reg s] (parse-long s))}

     :month {:unit   :month
             :parse  (fn [reg s] (system/useq-nth (registry/useq reg :month :year) {} (dec (parse-long s))))
             :format (fn [reg v] (inc (system/useq-index-of (registry/useq reg :month :year) {} v)))}

     :year {:unit  :year
            :parse (fn [_reg s] (parse-long s))}})

  (def user-formats
    {:DD {:element :day
          :width   2
          :pad     "0"}

     :MM {:element :month
          :width   2
          :pad     "0"}

     :YY {:element :year
          :parse   (fn [_reg s] (str "20" s))
          :width   2
          :pad     "0"}

     :YYYY {:element :year
            :width   4
            :pad     "0"}})

  (doseq [[format-name format-params] (concat base-formats user-formats)]
    (reg-format! reg_ format-name format-params))

  (t/testing "04/2005; 04/05"
    (def d {:year 2005, :month :apr})
    (def f [:MM "/" :YYYY])

    (t/is (= "2005" (format-unit @reg_ :YYYY d)))

    (t/is (= {:year 2005} (parse-unit @reg_ :YYYY "2005")))

    (t/is (= "05" (format-unit @reg_ :YY d)))

    (t/is (= {:year 2005} (parse-unit @reg_ :YY "05")))

    (t/is (= "04" (format-unit @reg_ :MM d)))

    (t/is (= {:month :apr} (parse-unit @reg_ :MM "04"))))

  (t/testing "Saturday, Apr 25, 2005"
    (defn day-of-week
      "y > 1752"
      [year month day]
      (let [t {:jan 0, :feb 3
               :mar 2, :apr 5, :may 0
               :jun 3, :jul 5, :aug 1
               :sep 4, :oct 6, :nov 2
               :dec 4}
            y (if (contains? #{:jan :feb} month)
                (dec year)
                year)]
        (nth [:sun :mon :tue :wed :thu :fri :sat]
             (rem (+ y
                     (int (/ y 4))
                     (- (int (/ y 100)))
                     (int (/ y 400))
                     (get t month)
                     day)
                  7))))

    (def weekday-names
      {:mon {:full "Monday"    :short "Mon"}
       :tue {:full "Tuesday"   :short "Tue"}
       :wed {:full "Wednesday" :short "Tue"}
       :thu {:full "Thursday"  :short "Thu"}
       :fri {:full "Friday"    :short "Fri"}
       :sat {:full "Saturday"  :short "Sat"}
       :sun {:full "Sunday"    :short "Sun"}})

    (reg-format! reg_
                 :month/weekday
                 {:units  #{:year :month :day}
                  :parse  (constantly nil)
                  :format (fn [_reg {:keys [year month day]}]
                            (get-in weekday-names [(day-of-week year month day) :full]))})

    (reg-format! reg_
                 :month/weekday-short
                 {:units  #{:year :month :day}
                  :parse  (constantly nil)
                  :format (fn [_reg {:keys [year month day]}]
                            (get-in weekday-names [(day-of-week year month day) :short]))})

    (reg-format! reg_
                 :month/short
                 {:unit   :month
                  :width  3
                  :pad    " "
                  :parse  (fn [_reg s] (keyword (str/lower-case s)))
                  :format (fn [_reg v] (str/capitalize (name v)))})

    (def d {:year 2005, :month :apr, :day 5})
    (def f [:month/weekday ", " :month/short " " :day ", " :year])

    (t/is (= "Apr" (format-unit @reg_ :month/short d)))

    (t/is (= {:month :apr} (parse-unit @reg_ :month/short "Apr")))

    (t/is (= "5" (format-unit @reg_ :day d)))

    (t/is (= {:day 5} (parse-unit @reg_ :day "5")))

    (t/is (= "Tuesday" (format-unit @reg_ :month/weekday d)))

    (t/is (= nil (parse-unit @reg_ :month/weekday "Tuesday")))

    (t/is (= "Tue" (format-unit @reg_ :month/weekday-short d)))

    (t/is (= nil (parse-unit @reg_ :month/weekday-short "Sat"))))

  (t/testing ":elements; m2y2 0f6m0t1 -> {:year 2022, :month :jun, :day 1}"
    (reg-format! reg_
                 :YYMMDD
                 {:elements #{:YY :MM :DD}
                  :width 6
                  :parse (fn [_reg s]
                           {:YY (subs s 0 2)
                            :MM (subs s 2 4)
                            :DD (subs s 4 6)})
                  :format (fn [_reg {:as arg :keys [YY MM DD]}]
                            (str YY MM DD))})

    (reg-format! reg_
                 ::my-format
                 {:element :YYMMDD
                  :width 12
                  :parse  (fn [_reg s]
                            (str/join (take-nth 2 (rest s))))
                  :format (fn [_reg YYMMDD]
                            (str/join (interleave "my fmt" YYMMDD)))})

    (def d {:year 2022, :month :jun, :day 1})

    (t/is (= "m2y2 0f6m0t1" (format-unit @reg_ ::my-format d)))

    (t/is (= d (parse-unit @reg_ ::my-format "m2y2 0f6m0t1"))))

  (t/testing ":units"
    (reg-format! reg_
                 ::read-string
                 {:units #{:day :month :year}
                  :parse (fn [_reg s] (read-string s))})

    (def d {:year 2022, :month :jun, :day 1})

    (t/is (= d (read-string (format-unit @reg_ ::read-string d))))

    (t/is (= d (parse-unit @reg_ ::read-string (str d)))))


  (t/testing "roman numeric YYYY•MM•DD"
    (defn int->roman [i]
      (clojure.pprint/cl-format nil "~@R" i))

    (defn roman->int [r]
      (->> (reverse (str/upper-case r))
           (map {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1})
           (partition-by identity)
           (map (partial apply +))
           (reduce #(if (< %1 %2) (+ %1 %2) (- %1 %2)))))

    (reg-format! reg_
                 :roman/year
                 {:unit   :year
                  :parse  (fn [_reg s] (roman->int s))
                  :format (fn [_reg i] (int->roman i))})

    (reg-format! reg_
                 :roman/month
                 {:element :month
                  :parse  (fn [_reg s] (str (roman->int s)))
                  :format (fn [_reg s] (int->roman (parse-long s)))})

    (reg-format! reg_
                 :roman/day
                 {:unit   :day
                  :parse  (fn [_reg s] (roman->int s))
                  :format (fn [_reg i] (int->roman i))})

    (def d {:year 2022, :month :jun, :day 1})

    (t/is (= "MMXXII" (format-unit @reg_ :roman/year d)))

    (t/is (= {:year 2022} (parse-unit @reg_ :roman/year "MMXXII")))

    (t/is (= "VI" (format-unit @reg_ :roman/month d)))

    (t/is (= {:month :jun} (parse-unit @reg_ :roman/month "VI")))

    (t/is (= "I" (format-unit @reg_ :roman/day d)))

    (t/is (= {:day 1} (parse-unit @reg_ :roman/day "I")))))
