(ns unit-map.impl.system-test
  (:require [unit-map.impl.system :as sut]
            [unit-map.core :as umap]
            [clojure.test :as t]))


(def treg_ (umap/new-registry))


(do ;;NOTE: useqs
  #_(def si-prefixes
      {:Y  24
       :Z  21
       :E  18
       :P  15
       :T  12
       :G  9
       :M  6
       :k  3
       :h  2
       :da 1
       :_  0
       :d  -1
       :c  -2
       :m  -3
       :μ  -6
       :n  -9
       :p  -12
       :f  -15
       :a  -18
       :z  -21
       :y  -24})


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


  (defn weekday [{:keys [weekday]}]
    (condp contains? weekday
      #{:mon :tue :wed :thu :fri} :workday
      #{:sat :sun}                :weekend))


  (defn season [{:keys [month]}]
    (condp contains? month
      #{:dec :jan :feb} :winter
      #{:mar :apr :may} :spring
      #{:jun :jul :aug} :summer
      #{:sep :oct :nov} :autumn))

  (umap/reg-useq! treg_ :unit :ns,   :useq #unit-map/useq[0 1 .. 999999999] :next-unit :sec)

  (umap/reg-useq! treg_ :unit :ns,   :useq #unit-map/useq[0 1 .. 999999] :next-unit :ms)
  (umap/reg-useq! treg_ :unit :ms,   :useq #unit-map/useq[0 1 .. 999] :next-unit :sec)
  (umap/reg-useq! treg_ :unit :sec,  :useq #unit-map/useq[0 1 .. 59] :next-unit :min)
  (umap/reg-useq! treg_ :unit :min,  :useq #unit-map/useq[0 1 .. 59] :next-unit :hour)
  (umap/reg-useq! treg_ :unit :hour, :useq #unit-map/useq[0 1 .. 23] :next-unit :day)

  (umap/reg-useq! treg_ :unit :ms,   :useq #unit-map/useq[0 1 .. ##Inf])
  (umap/reg-useq! treg_ :unit :ns,   :useq #unit-map/useq[0 1 .. ##Inf])
  (umap/reg-useq! treg_ :unit :sec,  :useq #unit-map/useq[0 1 .. ##Inf])
  (umap/reg-useq! treg_ :unit :hour, :useq #unit-map/useq[0 1 .. ##Inf])
  (umap/reg-useq! treg_ :unit :day,  :useq #unit-map/useq[0 1 .. ##Inf]) #_"NOTE: should start with 0 or with 1?"

  (umap/reg-useq! treg_ :unit :am-pm/hour,   :useq #unit-map/useq[12 1 2 .. 11] :next-unit :am-pm/period, :eq-unit :hour)
  (umap/reg-useq! treg_ :unit :am-pm/period, :useq #unit-map/useq[:am :pm] :next-unit :day)

  (umap/reg-useq! treg_ :unit :day,   :useq #unit-map/useq[1 2 .. days-in-month] :next-unit :month)
  (umap/reg-useq! treg_ :unit :month, :useq #unit-map/useq[:jan :feb  :mar :apr :may  :jun :jul :aug  :sep :oct :nov  :dec] :next-unit :year)
  (umap/reg-useq! treg_ :unit :year,  :useq #unit-map/useq[##-Inf .. -2 -1 1 2 .. ##Inf])

  (umap/reg-useq! treg_ :unit :weekday,  :useq #unit-map/useq[:mon :tue :wed :thu :fri :sat :sun] :next-unit :week, :eq-unit :day)
  (umap/reg-useq! treg_ :unit :week,     :useq #unit-map/useq[1 2 .. 52])
  (umap/reg-useq! treg_ :unit :weekpart, :useq #unit-map/useq[weekday] :eq-unit :weekday)
  (umap/reg-useq! treg_ :unit :season,   :useq #unit-map/useq[season] :eq-unit :month)

  (umap/reg-useq! treg_ :unit :mil,  :useq #unit-map/useq[0 1 .. 999 ] :next-unit :inch)
  (umap/reg-useq! treg_ :unit :inch, :useq #unit-map/useq[0 1 .. 11  ] :next-unit :foot)
  (umap/reg-useq! treg_ :unit :foot, :useq #unit-map/useq[0 1 .. 5279] :next-unit :mile)
  (umap/reg-useq! treg_ :unit :mile, :useq #unit-map/useq[0 1 .. ##Inf])

  (umap/reg-useq! treg_ :unit :mm, :useq #unit-map/useq[0 1 .. 9  ] :next-unit :cm)
  (umap/reg-useq! treg_ :unit :cm, :useq #unit-map/useq[0 1 .. 99 ] :next-unit :m)
  (umap/reg-useq! treg_ :unit :m,  :useq #unit-map/useq[0 1 .. 999] :next-unit :km)
  (umap/reg-useq! treg_ :unit :km, :useq #unit-map/useq[0 1 .. ##Inf])

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

  (umap/reg-useq! treg_ :unit :epoch,  :useq #unit-map/useq[:BC :AD]))

(do ;;NOTE: systems
  (def imperial (umap/reg-system! treg_ [:mil :inch :foot :mile]))
  (def metric   (umap/reg-system! treg_ [:mm :cm :m :km]))

  (def ms-hour    (umap/reg-system! treg_ [:ms :sec :min :hour]))
  (def ns-hour    (umap/reg-system! treg_ [:ns :sec :min :hour]))
  (def ns-ms-hour (umap/reg-system! treg_ [:ns :ms :sec :min :hour]))

  (def timestamp    (umap/reg-system! treg_ [:ms]))
  (def ns-timestamp (umap/reg-system! treg_ [:ns]))

  (def seconds    (umap/reg-system! treg_ [:ns :ms :sec]))
  (def ns-seconds (umap/reg-system! treg_ [:ns :sec]))

  (def ms-day    (umap/reg-system! treg_ [:ms :sec :min :hour :day]))
  (def ns-day    (umap/reg-system! treg_ [:ns :sec :min :hour :day]))
  (def ns-ms-day (umap/reg-system! treg_ [:ns :ms :sec :min :hour :day]))

  (def ms-day-am-pm    (umap/reg-system! treg_ [:ms :sec :min :am-pm/hour :am-pm/period :day]))
  (def ns-day-am-pm    (umap/reg-system! treg_ [:ns :sec :min :am-pm/hour :am-pm/period :day]))
  (def ns-ms-day-am-pm (umap/reg-system! treg_ [:ns :ms :sec :min :am-pm/hour :am-pm/period :day]))

  (def date       (umap/reg-system! treg_ [:day :month :year]))
  (def month-year (umap/reg-system! treg_ [:month :year]))

  (def ms-year    (umap/reg-system! treg_ [:ms :sec :min :hour :day :month :year]))
  (def ns-year    (umap/reg-system! treg_ [:ns :sec :min :hour :day :month :year]))
  (def ns-ms-year (umap/reg-system! treg_ [:ns :ms :sec :min :hour :day :month :year]))

  (def ms-year-am-pm    (umap/reg-system! treg_ [:ms :sec :min :am-pm/hour :am-pm/period :day :month :year]))
  (def ns-year-am-pm    (umap/reg-system! treg_ [:ns :sec :min :am-pm/hour :am-pm/period :day :month :year]))
  (def ns-ms-year-am-pm (umap/reg-system! treg_ [:ns :ms :sec :min :am-pm/hour :am-pm/period :day :month :year]))

  (def weeks (umap/reg-system! treg_ [:weekday :week]))

  (def ms-year-epoch (umap/reg-system! treg_ [:ms :sec :min :hour :day :month :epoch-year :epoch]))
  (def year-epoch    (umap/reg-system! treg_ [:epoch-year :epoch])))


(t/deftest system-detection
  (t/is (= ms-hour
           (sut/guess-system @treg_ {:min 30, :hour 15})))

  (t/is (= ms-day-am-pm
           (sut/guess-system @treg_ {:min 30, :am-pm/hour 3, :am-pm/period :pm})))

  (t/is (= ns-ms-day
           (sut/guess-system @treg_ {:ns 1, :ms 1, :sec 1, :min 1, :hour 1, :day 1})))

  (t/is (= ns-ms-hour
           (sut/guess-system @treg_ {:ns 1, :ms 1, :sec 1, :min 1, :hour 25})))

  (t/is (= ns-ms-hour
           (sut/guess-system @treg_ {:ns 1, :ms 1, :sec 1, :min 1501})))

  (t/is (= seconds
           (sut/guess-system @treg_ {:ns 1, :ms 1, :sec 90061})))

  (t/is (= seconds
           (sut/guess-system @treg_ {:ns 1, :ms 90061001})))

  (t/is (= ns-timestamp
           (sut/guess-system @treg_ {:ns 90061001000001})))


  (t/is (= ns-day
           (sut/guess-system @treg_ {:ns 1000001, :sec 1, :min 1, :hour 1, :day 1})))

  (t/is (= ns-hour
           (sut/guess-system @treg_ {:ns 1000001, :sec 1, :min 1, :hour 25})))

  (t/is (= ns-hour
           (sut/guess-system @treg_ {:ns 1000001, :sec 1, :min 1501})))

  (t/is (= ns-seconds
           (sut/guess-system @treg_ {:ns 1000001, :sec 90061})))

  (t/is (= ns-timestamp
           (sut/guess-system @treg_ {:ns 90061001000001})))

  (t/is (= ns-day
           (sut/guess-system @treg_ {:ns 1, :sec 1, :min 1, :hour 1, :day 1 :delta {:ns 1}})))


  (t/is (= ms-day
           (sut/guess-system @treg_ {:ms 1, :sec 1, :min 1, :hour 1, :day 1})))

  (t/is (= ms-hour
           (sut/guess-system @treg_ {:ms 1, :sec 1, :min 1, :hour 25})))

  (t/is (= ms-hour
           (sut/guess-system @treg_ {:ms 1, :sec 1, :min 1501})))

  (t/is (= ms-day
           (sut/guess-system @treg_ {:ms 1, :sec 1, :min 1501}
                          :day)))

  (t/is (= seconds
           (sut/guess-system @treg_ {:ms 1, :sec 90061})))

  (t/is (= timestamp
           (sut/guess-system @treg_ {:ms 90061001})))

  (t/is (= ms-day
           (sut/guess-system @treg_ {:ms 1, :sec 1, :min 1, :hour 1, :day 1 :delta {:ms 1}})))

  (t/is (nil? (sut/guess-system @treg_ {})))

  (t/is (nil? (sut/guess-system @treg_ nil))))


(t/deftest find-diff-branches-unit-test
  (sut/find-diff-branches [:ns :ms :sec :min :am-pm/hour :am-pm/period :day :month :year]
                          [:ns :sec :min :hour :period :day :month :year])
  ;; => [:ns
  ;;     [[:ms] []]
  ;;     :sec
  ;;     :min
  ;;     [[:am-pm/hour :am-pm/period] [:hour :period]]
  ;;     :day
  ;;     :month
  ;;     :year]

  (t/is (= [1 11 2 22 3 6 4 44 5 55]
           (sut/find-diff-branches [1 11 2 22 3 6 4 44 5 55]
                                   [1 11 2 22 3 6 4 44 5 55])))

  (t/is (= [1 11 [[2 22] [88]] 3 6 [[4 44] [99 9]] 5 55]
           (sut/find-diff-branches [1 11 2 22 3 6 4 44 5 55]
                                   [1 11 88 3 6 99 9 5 55])))

  (t/is (= [1 11 [[2] []] 3]
           (sut/find-diff-branches [1 11 2 3]
                                   [1 11 3])))

  (t/is (= [1 11 [[] [3]]]
           (sut/find-diff-branches [1 11]
                                   [1 11 3])))

  (t/is (= [1 11 [[4] [3]]]
           (sut/find-diff-branches [1 11 4]
                                   [1 11 3])))

  (t/is (= [[[] [1 2]] 11 3]
           (sut/find-diff-branches [11 3]
                                   [1 2 11 3])))

  (t/is (= [[[] [1]] 11 3]
           (sut/find-diff-branches [11 3]
                                   [1 11 3])))

  (t/is (= [[[] [1]] 11 3 [[] [4]]]
           (sut/find-diff-branches [11 3]
                                   [1 11 3 4])))

  (t/is (= [[[2] [1]] 11 3]
           (sut/find-diff-branches [2 11 3]
                                   [1 11 3])))

  (t/is (= [1 2 [[3] []] 4 5]
           (sut/find-diff-branches [1 2 3 4 5]
                                   [1 2 4 5])))

  (t/is (= [[[] [1 11 3]]]
           (sut/find-diff-branches []
                                   [1 11 3])))


  (t/is (= [1 11 [[88] [2 22]] 3 6 [[99 9] [4 44]] 5 55]
           (sut/find-diff-branches [1 11 88 3 6 99 9 5 55]
                                   [1 11 2 22 3 6 4 44 5 55])))

  (t/is (= [1 11 [[] [2]] 3]
           (sut/find-diff-branches [1 11 3]
                                   [1 11 2 3])))

  (t/is (= [1 11 [[3] []]]
           (sut/find-diff-branches [1 11 3]
                                   [1 11])))

  (t/is (= [[[1 2] []] 11 3]
           (sut/find-diff-branches [1 2 11 3]
                                   [11 3])))

  (t/is (= [[[1] []] 11 3]
           (sut/find-diff-branches [1 11 3]
                                   [11 3])))

  (t/is (= [[[1] []] 11 3 [[4] []]]
           (sut/find-diff-branches [1 11 3 4]
                                   [11 3])))


  (t/is (= [[[1 2] []] 3 4]
           (sut/find-diff-branches [1 2 3 4]
                                   [3 4])))

  (t/is (= [1 [[2] []] 3 4]
           (sut/find-diff-branches [1 2 3 4]
                                   [1 3 4])))

  (t/is (= [[[1 11 3] []]]
           (sut/find-diff-branches [1 11 3]
                                   [])))

  (t/is (= nil
           (sut/find-diff-branches []
                                   [])))

  (t/is (= [[[1 2 3] [:a :b :c]]]
           (sut/find-diff-branches [1 2 3] [:a :b :c]))))


(t/deftest system-conversion
  (t/testing "interseciton"
    (t/is (= ms-year
             (sut/system-intersection @treg_
                                   {:year 2021, :month :sep, :day 7, :hour 21, :min 30, :tz {:hour 2}}
                                   {:delta {:hour 3}})))

    (t/is (= ms-year
             (sut/system-intersection @treg_
                                   {:year 2021, :month :sep, :day 7, :hour 21, :min 30, :tz {:hour 2}}
                                   {:year 2021, :month :sep, :day 7, :hour 22, :min 30, :tz {:hour 3}})))

    (t/is (= ms-year
             (sut/system-intersection @treg_
                                   {:year 2021, :month :sep, :day 7, :hour 21, :min 30, :tz {:hour 2}}
                                   {:year 2021})))

    (t/is (nil? (sut/system-intersection @treg_
                                      {:year 2021, :month :sep, :day 7, :hour 21, :min 30, :tz {:hour 2}}
                                      {:cm 49})))

    (t/is (= ms-year
             (sut/system-intersection @treg_
                                   {:year 2021, :month :sep, :day 7, :hour 21, :min 30, :tz {:hour 2}}
                                   {})))

    (t/is (= ms-year
             (sut/system-intersection @treg_
                                   {:year 2021, :month :sep, :day 7, :hour 21, :min 30, :tz {:hour 2}}))))

  (t/testing "find conversion"
    #_"TODO: timezones"

    (t/is (= [{[:ms]    [:ms]}
              {[:sec]   [:sec]}
              {[:min]   [:min]}
              {[:hour]  [:hour]}
              {[:day]   [:day]}
              {[:month] [:month]}
              {[:year]  [:year]}]
             (sut/find-conversion
               @treg_
               {:year 2021, :month :sep, :day 7, :hour 21, :min 30}
               {:year 2021, :month :sep, :day 7, :hour 21, :min 30})))

    (t/is (= [{[:ms]    [:ms]}
              {[:sec]   [:sec]}
              {[:min]   [:min]}
              {[:hour]  [:am-pm/hour :am-pm/period]}
              {[:day]   [:day]}
              {[:month] [:month]}
              {[:year]  [:year]}]
             (sut/find-conversion
               @treg_
               {:year 2021, :month :sep, :day 7, :hour 21, :min 30}
               {:year 2021, :month :sep, :day 7, :am-pm/period :pm, :am-pm/hour 9, :min 30})))

    (t/testing "different start"
      (t/is (= [{[]       [:ns]}
                {[:ms]    [:ms]}
                {[:sec]   [:sec]}
                {[:min]   [:min]}
                {[:hour]  [:am-pm/hour :am-pm/period]}
                {[:day]   [:day]}
                {[:month] [:month]}
                {[:year]  [:year]}]
               (sut/find-conversion
                 @treg_
                 {:year 2021, :month :sep, :day 7, :hour 21, :min 30, :sec 10, :ms 10}
                 {:year 2021, :month :sep, :day 7, :am-pm/period :pm, :am-pm/hour 9, :min 30, :sec 10, :ms 10, :ns 10}))))

    (t/testing "two parallel graph paths"
      (t/is (= [{[:ns]    [:ns]}
                {[]       [:ms]}
                {[:sec]   [:sec]}
                {[:min]   [:min]}
                {[:hour]  [:am-pm/hour :am-pm/period]}
                {[:day]   [:day]}
                {[:month] [:month]}
                {[:year]  [:year]}]
               (sut/find-conversion
                 @treg_
                 {:year 2021, :month :sep, :day 7, :hour 21, :min 30, :sec 10, :ns 10000010}
                 {:year 2021, :month :sep, :day 7, :am-pm/period :pm, :am-pm/hour 9, :min 30, :sec 10, :ms 10, :ns 10}))))

    (t/testing "no conversion"
      (t/is (empty?
              (sut/find-conversion
                @treg_
                {:year 2021, :month :sep, :day 7, :hour 21, :min 30, :sec 10, :ns 10000010}
                {:m 1, :cm 82}))))

    (t/testing "no common units, no common finish"
      (t/is (= [{[:weekday :week] [:day :month :year]}]
               (sut/find-conversion
                 @treg_
                 {:week 6}
                 {:year 2022, :month :jan, :day 1}))))))


(t/deftest useq-urange-utils-test
  (t/testing "static? dynamic?"
    (t/is (true? (sut/static-useq? #unit-map/useq[0 1 .. 9])))

    (t/is (false? (sut/static-useq? #unit-map/useq[0 1 .. (fn [_] 9)])))

    (t/is (true? (sut/dynamic-useq? #unit-map/useq[0 1 .. (fn [_] 9)])))

    (t/is (false? (sut/dynamic-useq? #unit-map/useq[0 1 .. 9]))))

  (t/testing "concretize urange"
    (t/is (= {:start 0, :step 1, :end 9}
             (sut/concretize-urange (-> #unit-map/useq[(fn [_] 0) (fn [_] 1) .. (fn [_] 9)]
                                        first)
                                    nil)))

    (t/is (= {:start 0, :step 1, :end 9}
             (sut/concretize-urange (-> #unit-map/useq[0 1 .. 9]
                                        first)
                                    nil)))

    (t/is (= {:start 0, :step 1, :end 9}
             (sut/concretize-urange (-> #unit-map/useq[(fn [_] 0) .. (fn [_] 1) (fn [_] 9)]
                                        first)
                                    nil)))

    (t/is (= {:start 0, :step 1, :end 9}
             (sut/concretize-urange (-> #unit-map/useq[0 .. 8 9]
                                        first)
                                    nil)))

    (t/is (= {:start 1, :step 1, :end 28}
             (sut/concretize-urange (-> #unit-map/useq[1 2 .. (fn [{:keys [month]}] (if (= :feb month) 28 30))]
                                        first)
                                    {:day 1, :month :feb, :year 2022}))))

  (t/testing "useq length"
    (t/is (= 10 (sut/useq-length #unit-map/useq[0 1 .. 9]
                                 nil)))

    (t/is (= ##Inf (sut/useq-length #unit-map/useq[0 1 .. ##Inf]
                                    nil)))

    (t/is (= 10 (sut/useq-length #unit-map/useq[-9 -8 .. 0]
                                 nil)))

    (t/is (= 10 (sut/useq-length #unit-map/useq[-9 .. -1 0]
                                 nil)))

    (t/is (= ##Inf (sut/useq-length #unit-map/useq[##-Inf .. -1 0]
                                    nil)))

    (t/is (= ##Inf (sut/useq-length #unit-map/useq[##-Inf .. -1 0 1 2 .. ##Inf]
                                    nil))))

  (t/testing "first index"
    (t/is (= 0 (sut/useq-first-index #unit-map/useq[0 1 .. 9]
                                     nil)))

    (t/is (= 0 (sut/useq-first-index #unit-map/useq[0 1 .. ##Inf]
                                     nil)))

    (t/is (= ##-Inf (sut/useq-first-index #unit-map/useq[##-Inf .. -1 0]
                                          nil)))

    (t/is (= ##-Inf (sut/useq-first-index #unit-map/useq[##-Inf .. -1 0 1 2 .. ##Inf]
                                          nil))))

  (t/testing "last index"
    (t/is (= 9 (sut/useq-last-index #unit-map/useq[0 1 .. 9]
                                    nil)))

    (t/is (= 11 (sut/useq-last-index #unit-map/useq[0 1 .. 9 10 11]
                                     nil)))

    (t/is (= 11 (sut/useq-last-index #unit-map/useq[-2 -1 0 1 .. 9]
                                     nil)))

    (t/is (= ##Inf (sut/useq-last-index #unit-map/useq[-1 0 1 .. ##Inf]
                                        nil)))

    (t/is (= ##Inf #_"TODO: probably should be 1"
             (sut/useq-last-index
               #unit-map/useq[##-Inf .. -1 0 1]
               nil)))

    (t/is (= ##Inf (sut/useq-last-index #unit-map/useq[##-Inf .. -1 0 1 2 .. ##Inf]
                                        nil))))

  (t/testing "contains"
    (t/is (some? (sut/useq-contains-some
                   #unit-map/useq[##-Inf .. -2 -1 1 2 3 .. ##Inf]
                   nil
                   10)))

    (t/is (some? (sut/useq-contains-some
                   #unit-map/useq[##-Inf .. -2 -1 1 2 3 .. ##Inf]
                   nil
                   -10)))

    (t/is (some? (sut/useq-contains-some
                   #unit-map/useq[##-Inf .. -2 -1 1 2 3 .. ##Inf]
                   nil
                   1)))

    (t/is (some? (sut/useq-contains-some
                   #unit-map/useq[##-Inf .. -2 -1 1 2 3 .. ##Inf]
                   nil
                   ##Inf)))

    (t/is (some? (sut/useq-contains-some
                   #unit-map/useq[##-Inf .. -2 -1 1 2 3 .. ##Inf]
                   nil
                   ##-Inf)))

    (t/is (nil? (sut/useq-contains-some
                  #unit-map/useq[##-Inf .. -2 -1 1 2 3 .. ##Inf]
                  nil
                  0))))

  (t/testing "index-of"
    (t/is (= 11 (sut/useq-index-of #unit-map/useq[##-Inf .. -3 -2 -1 1 2 3 .. ##Inf]
                                   nil
                                   10)))

    (t/is (= -8 (sut/useq-index-of #unit-map/useq[##-Inf .. -3 -2 -1 1 2 3 .. ##Inf]
                                   nil
                                   -10)))

    (t/is (= 2 (sut/useq-index-of #unit-map/useq[##-Inf .. -3 -2 -1 1 2 3 .. ##Inf]
                                  nil
                                  1)))

    (t/is (= ##Inf (sut/useq-index-of #unit-map/useq[##-Inf .. -3 -2 -1 1 2 3 .. ##Inf]
                                      nil
                                      ##Inf)))

    (t/is (= ##-Inf (sut/useq-index-of #unit-map/useq[##-Inf .. -3 -2 -1 1 2 3 .. ##Inf]
                                       nil
                                       ##-Inf))))

  (t/testing "nth"
    (t/testing "index-of"
      (t/is (= 10 (sut/useq-nth #unit-map/useq[##-Inf .. -3 -2 -1 1 2 3 .. ##Inf]
                                nil
                                11)))

      (t/is (= -10 (sut/useq-nth #unit-map/useq[##-Inf .. -3 -2 -1 1 2 3 .. ##Inf]
                                 nil
                                 -8)))

      (t/is (= 1 (sut/useq-nth #unit-map/useq[##-Inf .. -3 -2 -1 1 2 3 .. ##Inf]
                               nil
                               2)))

      (t/is (= ##Inf (sut/useq-nth #unit-map/useq[##-Inf .. -3 -2 -1 1 2 3 .. ##Inf]
                                   nil
                                   ##Inf)))

      (t/is (= ##-Inf (sut/useq-nth #unit-map/useq[##-Inf .. -3 -2 -1 1 2 3 .. ##Inf]
                                    nil
                                    ##-Inf))))))


(t/deftest system-utils-test
  (t/testing "next/prev unit"
    (t/is (= :month
             (sut/get-next-unit @treg_
                                {:year 2022 :month :jun :day 4 :hour 12 :min 30}
                                :day)))

    (t/is (= :hour
             (sut/get-prev-unit @treg_
                                {:year 2022 :month :jun :day 4 :hour 12 :min 30}
                                :day)))

    (t/is (= nil
             (sut/get-next-unit @treg_
                                {:year 2022 :month :jun :day 4 :hour 12 :min 30}
                                :year)))

    (t/is (= :sec
             (sut/get-prev-unit @treg_
                                {:year 2022 :month :jun :day 4 :hour 12 :min 30}
                                :min)))

    (t/is (= :ms
             (sut/get-prev-unit @treg_
                                {:year 2022 :month :jun :day 4 :hour 12 :min 30}
                                :sec)))

    (t/is (= nil
             (sut/get-prev-unit @treg_
                                {:year 2022 :month :jun :day 4 :hour 12 :min 30}
                                :ms)))

    (t/is (= :year
             (sut/get-next-unit @treg_
                                {:min 30}
                                :month)))

    (t/is (= :day
             (sut/get-prev-unit @treg_
                                {:min 30}
                                :month))))

  (t/testing "get-useq"
    (t/is (= [:jan :feb  :mar :apr :may  :jun :jul :aug  :sep :oct :nov  :dec]
             (sut/get-useq @treg_
                           {:year 2022 :month :jun :day 4 :hour 12 :min 30}
                           :month)))

    (t/is (= [:jan :feb  :mar :apr :may  :jun :jul :aug  :sep :oct :nov  :dec]
             (sut/get-useq @treg_
                           {:min 30}
                           :month)))

    (t/is (= #unit-map/useq[##-Inf .. -2 -1 1 2 .. ##Inf]
             (sut/get-useq @treg_
                           {:year 2022 :month :jun :day 4 :hour 12 :min 30}
                           :year))))

  (t/testing "get-next-unit-value"
    (t/is (= (range 60)
             (->> (iterate #(sut/get-next-unit-value
                              (get-in @treg_ [:useqs :sec :min :useq])
                              nil
                              %)
                           0)
                  (take-while some?))))

    (t/is (= [:jan :feb  :mar :apr :may  :jun :jul :aug  :sep :oct :nov  :dec]
             (->> (iterate #(sut/get-next-unit-value
                              (get-in @treg_ [:useqs :month :year :useq])
                              nil
                              %)
                           :jan)
                  (take-while some?))))

    (t/is (= (range 1970 2021)
             (->> (iterate #(sut/get-next-unit-value
                              (get-in @treg_ [:useqs :year nil :useq])
                              nil
                              %)
                           1970)
                  (take 51))))

    (t/is (= [12 1 2 3 4 5 6 7 8 9 10 11]
             (->> (iterate #(sut/get-next-unit-value
                              (get-in @treg_ [:useqs :am-pm/hour :am-pm/period :useq])
                              nil
                              %)
                           12)
                  (take-while some?))))

    (t/is (= 13 (sut/get-next-unit-value #unit-map/useq[1 3 .. :TODO/remove (fn [{:keys [bar]}] (if (odd? bar) 9 11)) 13 15]
                                         {:bar 7}
                                         9)))

    (t/is (= 11 (sut/get-next-unit-value #unit-map/useq[1 3 .. :TODO/remove (fn [{:keys [bar]}] (if (odd? bar) 9 11)) 13 15]
                                         {:bar 8}
                                         9))))

  (t/testing "get-prev-unit-value"
    (t/is (= (reverse (range 60))
             (->> (iterate #(sut/get-prev-unit-value
                              (get-in @treg_ [:useqs :sec :min :useq])
                              nil
                              %)
                           59)
                  (take-while some?))))

    (t/is (= (reverse [:jan :feb  :mar :apr :may  :jun :jul :aug  :sep :oct :nov  :dec])
             (->> (iterate #(sut/get-prev-unit-value
                              (get-in @treg_ [:useqs :month :year :useq])
                              nil
                              %)
                           :dec)
                  (take-while some?))))

    (t/is (= (reverse (range 1970 2021))
             (->> (iterate #(sut/get-prev-unit-value
                              (get-in @treg_ [:useqs :year nil :useq])
                              nil
                              %)
                           2020)
                  (take 51))))

    (t/is (= (reverse [12 1 2 3 4 5 6 7 8 9 10 11])
             (->> (iterate #(sut/get-prev-unit-value
                              (get-in @treg_ [:useqs :am-pm/hour :am-pm/period :useq])
                              nil
                              %)
                           11)
                  (take-while some?))))

    (t/is (= 9 (sut/get-prev-unit-value #unit-map/useq[1 3 .. :TODO/remove (fn [{:keys [bar]}] (if (odd? bar) 9 11)) 13 15]
                                        {:bar 7}
                                        13)))

    (t/is (= 11 (sut/get-prev-unit-value #unit-map/useq[1 3 .. :TODO/remove (fn [{:keys [bar]}] (if (odd? bar) 9 11)) 13 15]
                                         {:bar 8}
                                         13))))

  (t/testing "get first/last el"
    (t/is (= 0 (sut/get-first-el #unit-map/useq[0 1 3 .. :TODO/remove (fn [{:keys [bar]}] (if (odd? bar) 9 11)) 13 15]
                                 {})))

    (t/is (= 15 (sut/get-last-el #unit-map/useq[0 1 3 .. :TODO/remove (fn [{:keys [bar]}] (if (odd? bar) 9 11)) 13 15]
                                 {})))

    (t/is (= 1 (sut/get-first-el #unit-map/useq[(constantly 1) (constantly 1) .. :TODO/remove (constantly 10)]
                                 {})))

    (t/is (= 10 (sut/get-last-el #unit-map/useq[(constantly 1) (constantly 1) .. :TODO/remove (constantly 10)]
                                 {}))))

  (t/testing "get min/max value"
    (t/is (= ##-Inf (sut/get-min-value @treg_ {:year 2022} :year)))

    (t/is (= ##Inf (sut/get-max-value @treg_ {:year 2022} :year)))))
