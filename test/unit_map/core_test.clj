(ns unit-map.core-test
  (:require [unit-map.core :as sut]
            [clojure.test :as t]))


(t/deftest defseq-defsys
  (def tctx (atom nil))

  (sut/defseq* tctx :a #unit-map/seq[0 1 -> :b])
  (sut/defseq* tctx :b #unit-map/seq[0 1 -> :c])
  (sut/defseq* tctx :c #unit-map/seq[0 1 -> :d])
  (sut/defseq* tctx :d #unit-map/seq[0 1])

  (sut/defseq* tctx :aa #unit-map/seq[0 2 -> :b])
  (sut/defseq* tctx :a #unit-map/seq[0 1 2 3 -> :c])

  (sut/defseq* tctx :b2 #unit-map/seq[:b <=> -2 -1 0 -> :c2])
  (sut/defseq* tctx :c2 #unit-map/seq[-2 -1 0 -> :d])
  (sut/defseq* tctx :c2 #unit-map/seq[-2 -1 .. ##-Inf])

  (sut/defseq* tctx :b3 #unit-map/seq[:b2 <=> 2 1 0 -> :c3])
  (sut/defseq* tctx :c3 #unit-map/seq[2 1 .. ##-Inf])

  (sut/defseq* tctx :b4 #unit-map/seq[:b <=> 2 1 0 -> :c4])
  (sut/defseq* tctx :c4 #unit-map/seq[2 1 .. ##-Inf])

  (sut/defseq* tctx :b5 #unit-map/seq[2 1 0 -> :c5])
  (sut/defseq* tctx :c5 #unit-map/seq[2 1 .. ##-Inf])

  (sut/defseq* tctx :b6 #unit-map/seq[:b <=> 2 1 0 -> :c6])
  (sut/defseq* tctx :c6 #unit-map/seq[:c <=> 2 1 0 -> :d])
  (sut/defseq* tctx :c6 #unit-map/seq[2 1 .. ##-Inf])

  (t/testing "seqs graph"
    (def graph-assert
      {:a {:b  {:sequence [0 1], :unit :a, :next-unit :b}
           :b2 {:sequence [0 1], :unit :a, :next-unit :b2}
           :b3 {:sequence [0 1], :unit :a, :next-unit :b3}
           :b4 {:sequence [0 1], :unit :a, :next-unit :b4}
           :b6 {:sequence [0 1], :unit :a, :next-unit :b6}
           :c  {:sequence [0 1 2 3], :unit :a, :next-unit :c}
           :c6 {:sequence [0 1 2 3], :unit :a, :next-unit :c6}}
       :b {:c  {:sequence [0 1], :unit :b, :next-unit :c}
           :c6 {:sequence [0 1], :unit :b, :next-unit :c6}}
       :c {:d  {:sequence [0 1], :unit :c, :next-unit :d}}
       :d {nil {:sequence [0 1], :unit :d}}

       :aa {:b  {:sequence [0 2], :unit :aa, :next-unit :b}
            :b2 {:sequence [0 2], :unit :aa, :next-unit :b2}
            :b3 {:sequence [0 2], :unit :aa, :next-unit :b3}
            :b4 {:sequence [0 2], :unit :aa, :next-unit :b4}
            :b6 {:sequence [0 2], :unit :aa, :next-unit :b6}}

       :b2 {:c2 {:sequence [-2 -1 0], :unit :b2, :next-unit :c2}}
       :c2 {:d {:sequence [-2 -1 0], :unit :c2, :next-unit :d}
            nil {:sequence [{:start -2, :step 1, :end ##-Inf}], :unit :c2}}

       :b3 {:c3 {:sequence [2 1 0], :unit :b3, :next-unit :c3}}
       :c3 {nil {:sequence [{:start 2, :step -1, :end ##-Inf}], :unit :c3}}

       :b4 {:c4 {:sequence [2 1 0], :unit :b4, :next-unit :c4}}
       :c4 {nil {:sequence [{:start 2, :step -1, :end ##-Inf}], :unit :c4}}

       :b5 {:c5 {:sequence [2 1 0], :unit :b5, :next-unit :c5}}
       :c5 {nil {:sequence [{:start 2, :step -1, :end ##-Inf}], :unit :c5}}

       :b6 {:c6 {:sequence [2 1 0], :unit :b6, :next-unit :c6}
            :c  {:sequence [2 1 0], :unit :b6, :next-unit :c}}
       :c6 {:d {:sequence [2 1 0], :unit :c6, :next-unit :d}
            nil {:sequence [{:start 2, :step -1, :end ##-Inf}], :unit :c6}}})

    (t/is (= graph-assert (:seqs @tctx)))

    (t/is (= #{#{:a} #{:aa}
               #{:b :b2 :b3 :b4 :b6} #{:b5}
               #{:c :c6} #{:c2} #{:c3} #{:c4} #{:c5}
               #{:d}}
             (:eq-units @tctx))))

  (t/testing "valid systems"
    (t/is (sut/sys-continuous?* tctx [:a :b :c :d]))
    (t/is (sut/sys-continuous?* tctx [:a :b2 :c2 :d]))
    (t/is (sut/sys-continuous?* tctx [:a :b2 :c2]))
    (t/is (sut/sys-continuous?* tctx [:a :b3 :c3]))
    (t/is (sut/sys-continuous?* tctx [:a :b4 :c4]))
    (t/is (sut/sys-continuous?* tctx [:b5 :c5]))
    (t/is (sut/sys-continuous?* tctx [:a :b6 :c6 :d]))
    (t/is (sut/sys-continuous?* tctx [:a :b6 :c6]))
    (t/is (sut/sys-continuous?* tctx [:a :b :c6 :d])))

  (t/testing "invalid systems"
    (t/is (not (sut/sys-continuous?* tctx [:d :c :b :a])))
    (t/is (not (sut/sys-continuous?* tctx [:a :b2 :c])))
    (t/is (not (sut/sys-continuous?* tctx [:a :b3 :c3 :d])))

    (t/is (not (sut/sys-continuous?* tctx [:a])))))


(do ;;NOTE: seqs
  (def si-prefixes
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

  (sut/defseq :ns   #unit-map/seq[0 1 .. 999999999 -> :sec])

  (sut/defseq :ns   #unit-map/seq[0 1 .. 999999 -> :ms])
  (sut/defseq :ms   #unit-map/seq[0 1 .. 999 -> :sec])
  (sut/defseq :sec  #unit-map/seq[0 1 .. 59 -> :min])
  (sut/defseq :min  #unit-map/seq[0 1 .. 59 -> :hour])
  (sut/defseq :hour #unit-map/seq[0 1 .. 23 -> :day])

  (sut/defseq :ms   #unit-map/seq[0 1 .. ##Inf])
  (sut/defseq :ns   #unit-map/seq[0 1 .. ##Inf])
  (sut/defseq :sec  #unit-map/seq[0 1 .. ##Inf])
  (sut/defseq :hour #unit-map/seq[0 1 .. ##Inf])
  (sut/defseq :day  #unit-map/seq[0 1 .. ##Inf]) #_"NOTE: should start with 0 or with 1?"

  (sut/defseq :am-pm/hour   #unit-map/seq[:hour <=> 12 1 2 .. 11 -> :am-pm/period])
  (sut/defseq :am-pm/period #unit-map/seq[:am :pm -> :day])

  (sut/defseq :day   #unit-map/seq[1 2 .. days-in-month -> :month])
  (sut/defseq :month #unit-map/seq[:jan :feb  :mar :apr :may  :jun :jul :aug  :sep :oct :nov  :dec -> :year])
  (sut/defseq :year  #unit-map/seq[##-Inf .. -2 -1 1 2 .. ##Inf])

  (sut/defseq :weekday  #unit-map/seq[:day <=> :mon :tue :wed :thu :fri :sat :sun -> :week])
  (sut/defseq :week     #unit-map/seq[1 2 .. 52])
  (sut/defseq :weekpart #unit-map/seq[:weekday <=> weekday])
  (sut/defseq :season   #unit-map/seq[:month <=> season])

  (sut/defseq :mil  #unit-map/seq[0 1 .. 999  -> :inch])
  (sut/defseq :inch #unit-map/seq[0 1 .. 11   -> :foot])
  (sut/defseq :foot #unit-map/seq[0 1 .. 5279 -> :mile])
  (sut/defseq :mile #unit-map/seq[0 1 .. ##Inf])

  (sut/defseq :mm #unit-map/seq[0 1 .. 9   -> :cm])
  (sut/defseq :cm #unit-map/seq[0 1 .. 99  -> :m])
  (sut/defseq :m  #unit-map/seq[0 1 .. 999 -> :km])
  (sut/defseq :km #unit-map/seq[0 1 .. ##Inf]))

(do ;;NOTE: systems
  (sut/defsys imperial [:mil :inch :foot :mile])

  (sut/defsys metric   [:mm :cm :m :km])

  (sut/defsys ms-hour    [:ms :sec :min :hour])
  (sut/defsys ns-hour    [:ns :sec :min :hour])
  (sut/defsys ns-ms-hour [:ns :ms :sec :min :hour])

  (sut/defsys timestamp    [:ms])
  (sut/defsys ns-timestamp [:ns])

  (sut/defsys seconds [:ns :ms :sec])
  (sut/defsys ns-seconds [:ns :sec])

  (sut/defsys ms-day    [:ms :sec :min :hour :day])
  (sut/defsys ns-day    [:ns :sec :min :hour :day])
  (sut/defsys ns-ms-day [:ns :ms :sec :min :hour :day])

  (sut/defsys ms-day-am-pm    [:ms :sec :min :am-pm/hour :am-pm/period :day])
  (sut/defsys ns-day-am-pm    [:ns :sec :min :am-pm/hour :am-pm/period :day])
  (sut/defsys ns-ms-day-am-pm [:ns :ms :sec :min :am-pm/hour :am-pm/period :day])

  (sut/defsys date       [:day :month :year])
  (sut/defsys month-year [:month :year])

  (sut/defsys ms-year    [:ms :sec :min :hour :day :month :year])
  (sut/defsys ns-year    [:ns :sec :min :hour :day :month :year])
  (sut/defsys ns-ms-year [:ns :ms :sec :min :hour :day :month :year])

  (sut/defsys ms-year-am-pm    [:ms :sec :min :am-pm/hour :am-pm/period :day :month :year])
  (sut/defsys ns-year-am-pm    [:ns :sec :min :am-pm/hour :am-pm/period :day :month :year])
  (sut/defsys ns-ms-year-am-pm [:ns :ms :sec :min :am-pm/hour :am-pm/period :day :month :year])

  (sut/defsys weeks [:weekday :week]))


(t/deftest sys-detection
  (t/is (= ms-hour
           (first (sut/guess-sys {:min 30, :hour 15}))))

  (t/is (= ms-day-am-pm
           (first (sut/guess-sys {:min 30, :am-pm/hour 3, :am-pm/period :pm}))))

  (t/is (= ns-ms-day
           (first (sut/guess-sys {:ns 1, :ms 1, :sec 1, :min 1, :hour 1, :day 1}))))

  (t/is (= ns-ms-hour
           (first (sut/guess-sys {:ns 1, :ms 1, :sec 1, :min 1, :hour 25}))))

  (t/is (= ns-ms-hour
           (first (sut/guess-sys {:ns 1, :ms 1, :sec 1, :min 1501}))))

  (t/is (= seconds
           (first (sut/guess-sys {:ns 1, :ms 1, :sec 90061}))))

  (t/is (= seconds
           (first (sut/guess-sys {:ns 1, :ms 90061001}))))

  (t/is (= ns-timestamp
           (first (sut/guess-sys {:ns 90061001000001}))))


  (t/is (= ns-day
           (first (sut/guess-sys {:ns 1000001, :sec 1, :min 1, :hour 1, :day 1}))))

  (t/is (= ns-hour
           (first (sut/guess-sys {:ns 1000001, :sec 1, :min 1, :hour 25}))))

  (t/is (= ns-hour
           (first (sut/guess-sys {:ns 1000001, :sec 1, :min 1501}))))

  (t/is (= ns-seconds
           (first (sut/guess-sys {:ns 1000001, :sec 90061}))))

  (t/is (= ns-timestamp
           (first (sut/guess-sys {:ns 90061001000001}))))

  (t/is (= ns-day
           (first (sut/guess-sys {:ns 1, :sec 1, :min 1, :hour 1, :day 1 :delta {:ns 1}}))))


  (t/is (= ms-day
           (first (sut/guess-sys {:ms 1, :sec 1, :min 1, :hour 1, :day 1}))))

  (t/is (= ms-hour
           (first (sut/guess-sys {:ms 1, :sec 1, :min 1, :hour 25}))))

  (t/is (= ms-hour
           (first (sut/guess-sys {:ms 1, :sec 1, :min 1501}))))

  (t/is (= ms-day
           (first (sut/guess-sys {:ms 1, :sec 1, :min 1501}
                                 :day))))

  (t/is (= seconds
           (first (sut/guess-sys {:ms 1, :sec 90061}))))

  (t/is (= timestamp
           (first (sut/guess-sys {:ms 90061001}))))

  (t/is (= ms-day
           (first (sut/guess-sys {:ms 1, :sec 1, :min 1, :hour 1, :day 1 :delta {:ms 1}}))))

  (t/is (empty? (sut/guess-sys {})))

  (t/is (empty? (sut/guess-sys nil))))


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


(t/deftest sys-conversion
  (t/testing "interseciton"
    (t/is (= [ms-year ns-year ns-ms-year]
             (sut/sys-intersection
               {:year 2021, :month :sep, :day 7, :hour 21, :min 30, :tz {:hour 2}}
               {:delta {:hour 3}})))

    (t/is (= [ms-year ns-year ns-ms-year]
             (sut/sys-intersection
               {:year 2021, :month :sep, :day 7, :hour 21, :min 30, :tz {:hour 2}}
               {:year 2021, :month :sep, :day 7, :hour 22, :min 30, :tz {:hour 3}})))

    (t/is (= [ms-year ns-year ns-ms-year]
             (sut/sys-intersection
               {:year 2021, :month :sep, :day 7, :hour 21, :min 30, :tz {:hour 2}}
               {:year 2021})))

    (t/is (empty? (sut/sys-intersection
                    {:year 2021, :month :sep, :day 7, :hour 21, :min 30, :tz {:hour 2}}
                    {:cm 49})))

    (t/is (empty? (sut/sys-intersection
                    {:year 2021, :month :sep, :day 7, :hour 21, :min 30, :tz {:hour 2}}
                    {})))

    (t/is (= [ms-year ns-year ns-ms-year]
             (sut/sys-intersection
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
                 {:year 2021, :month :sep, :day 7, :hour 21, :min 30, :sec 10, :ns 10000010}
                 {:year 2021, :month :sep, :day 7, :am-pm/period :pm, :am-pm/hour 9, :min 30, :sec 10, :ms 10, :ns 10}))))

    (t/testing "no conversion"
      (t/is (empty?
              (sut/find-conversion
                {:year 2021, :month :sep, :day 7, :hour 21, :min 30, :sec 10, :ns 10000010}
                {:m 1, :cm 82}))))

    (t/testing "no common units, no common finish"
      (t/is (= [{[:weekday :week] [:day :month :year]}]
               (sut/find-conversion
                 {:week 6}
                 {:year 2022, :month :jan, :day 1}))))))


(t/deftest seq-range-utils-test
  (t/testing "static? dynamic?"
    (t/is (true? (sut/static-sequence? #unit-map/seq[0 1 .. 9])))

    (t/is (false? (sut/static-sequence? #unit-map/seq[0 1 .. (fn [_] 9)])))

    (t/is (true? (sut/dynamic-sequence? #unit-map/seq[0 1 .. (fn [_] 9)])))

    (t/is (false? (sut/dynamic-sequence? #unit-map/seq[0 1 .. 9]))))

  (t/testing "concretize range"
    (t/is (= {:start 0, :step 1, :end 9}
             (sut/concretize-range (-> #unit-map/seq[(fn [_] 0) (fn [_] 1) .. (fn [_] 9)]
                                       :sequence
                                       first)
                                   nil)))

    (t/is (= {:start 0, :step 1, :end 9}
             (sut/concretize-range (-> #unit-map/seq[0 1 .. 9]
                                       :sequence
                                       first)
                                   nil)))

    (t/is (= {:start 0, :step 1, :end 9}
             (sut/concretize-range (-> #unit-map/seq[(fn [_] 0) .. (fn [_] 1) (fn [_] 9)]
                                       :sequence
                                       first)
                                   nil)))

    (t/is (= {:start 0, :step 1, :end 9}
             (sut/concretize-range (-> #unit-map/seq[0 .. 8 9]
                                       :sequence
                                       first)
                                   nil)))

    (t/is (= {:start 1, :step 1, :end 28}
             (sut/concretize-range (-> #unit-map/seq[1 2 .. (fn [{:keys [month]}] (if (= :feb month) 28 30))]
                                       :sequence
                                       first)
                                   {:day 1, :month :feb, :year 2022}))))

  (t/testing "seq length"
    (t/is (= 10 (sut/sequence-length #unit-map/seq[0 1 .. 9]
                                     nil)))

    (t/is (= ##Inf (sut/sequence-length #unit-map/seq[0 1 .. ##Inf]
                                        nil)))

    (t/is (= 10 (sut/sequence-length #unit-map/seq[-9 -8 .. 0]
                                     nil)))

    (t/is (= 10 (sut/sequence-length #unit-map/seq[-9 .. -1 0]
                                     nil)))

    (t/is (= ##Inf (sut/sequence-length #unit-map/seq[##-Inf .. -1 0]
                                        nil)))

    (t/is (= ##Inf (sut/sequence-length #unit-map/seq[##-Inf .. -1 0 1 2 .. ##Inf]
                                        nil))))

  (t/testing "first index"
    (t/is (= 0 (sut/sequence-first-index #unit-map/seq[0 1 .. 9]
                                         nil)))

    (t/is (= 0 (sut/sequence-first-index #unit-map/seq[0 1 .. ##Inf]
                                         nil)))

    (t/is (= ##-Inf (sut/sequence-first-index #unit-map/seq[##-Inf .. -1 0]
                                              nil)))

    (t/is (= ##-Inf (sut/sequence-first-index #unit-map/seq[##-Inf .. -1 0 1 2 .. ##Inf]
                                              nil))))

  (t/testing "last index"
    (t/is (= 9 (sut/sequence-last-index #unit-map/seq[0 1 .. 9]
                                        nil)))

    (t/is (= 11 (sut/sequence-last-index #unit-map/seq[0 1 .. 9 10 11]
                                         nil)))

    (t/is (= 11 (sut/sequence-last-index #unit-map/seq[-2 -1 0 1 .. 9]
                                         nil)))

    (t/is (= ##Inf (sut/sequence-last-index #unit-map/seq[-1 0 1 .. ##Inf]
                                            nil)))

    (t/is (= ##Inf #_"TODO: probably should be 1"
             (sut/sequence-last-index
               #unit-map/seq[##-Inf .. -1 0 1]
               nil)))

    (t/is (= ##Inf (sut/sequence-last-index #unit-map/seq[##-Inf .. -1 0 1 2 .. ##Inf]
                                            nil))))

  (t/testing "contains"
    (t/is (some? (sut/sequence-contains-some
                   #unit-map/seq[##-Inf .. -2 -1 1 2 3 .. ##Inf]
                   nil
                   10)))

    (t/is (some? (sut/sequence-contains-some
                   #unit-map/seq[##-Inf .. -2 -1 1 2 3 .. ##Inf]
                   nil
                   -10)))

    (t/is (some? (sut/sequence-contains-some
                   #unit-map/seq[##-Inf .. -2 -1 1 2 3 .. ##Inf]
                   nil
                   1)))

    (t/is (some? (sut/sequence-contains-some
                   #unit-map/seq[##-Inf .. -2 -1 1 2 3 .. ##Inf]
                   nil
                   ##Inf)))

    (t/is (some? (sut/sequence-contains-some
                   #unit-map/seq[##-Inf .. -2 -1 1 2 3 .. ##Inf]
                   nil
                   ##-Inf)))

    (t/is (nil? (sut/sequence-contains-some
                  #unit-map/seq[##-Inf .. -2 -1 1 2 3 .. ##Inf]
                  nil
                  0))))

  (t/testing "index-of"
    (t/is (= 11 (sut/sequence-index-of #unit-map/seq[##-Inf .. -3 -2 -1 1 2 3 .. ##Inf]
                                       nil
                                       10)))

    (t/is (= -8 (sut/sequence-index-of #unit-map/seq[##-Inf .. -3 -2 -1 1 2 3 .. ##Inf]
                                       nil
                                       -10)))

    (t/is (= 2 (sut/sequence-index-of #unit-map/seq[##-Inf .. -3 -2 -1 1 2 3 .. ##Inf]
                                      nil
                                      1)))

    (t/is (= ##Inf (sut/sequence-index-of #unit-map/seq[##-Inf .. -3 -2 -1 1 2 3 .. ##Inf]
                                          nil
                                          ##Inf)))

    (t/is (= ##-Inf (sut/sequence-index-of #unit-map/seq[##-Inf .. -3 -2 -1 1 2 3 .. ##Inf]
                                           nil
                                           ##-Inf))))

  (t/testing "nth"
    (t/testing "index-of"
      (t/is (= 10 (sut/sequence-nth #unit-map/seq[##-Inf .. -3 -2 -1 1 2 3 .. ##Inf]
                                    nil
                                    11)))

      (t/is (= -10 (sut/sequence-nth #unit-map/seq[##-Inf .. -3 -2 -1 1 2 3 .. ##Inf]
                                     nil
                                     -8)))

      (t/is (= 1 (sut/sequence-nth #unit-map/seq[##-Inf .. -3 -2 -1 1 2 3 .. ##Inf]
                                   nil
                                   2)))

      (t/is (= ##Inf (sut/sequence-nth #unit-map/seq[##-Inf .. -3 -2 -1 1 2 3 .. ##Inf]
                                       nil
                                       ##Inf)))

      (t/is (= ##-Inf (sut/sequence-nth #unit-map/seq[##-Inf .. -3 -2 -1 1 2 3 .. ##Inf]
                                        nil
                                        ##-Inf))))))


(t/deftest sys-utils-test
  (t/testing "next/prev unit"
    (t/is (= :month
             (sut/get-next-unit {:year 2022 :month :jun :day 4 :hour 12 :min 30}
                                :day)))

    (t/is (= :hour
             (sut/get-prev-unit {:year 2022 :month :jun :day 4 :hour 12 :min 30}
                                :day)))

    (t/is (= nil
             (sut/get-next-unit {:year 2022 :month :jun :day 4 :hour 12 :min 30}
                                :year)))

    (t/is (= :sec
             (sut/get-prev-unit {:year 2022 :month :jun :day 4 :hour 12 :min 30}
                                :min)))

    (t/is (= :ms
             (sut/get-prev-unit {:year 2022 :month :jun :day 4 :hour 12 :min 30}
                                :sec)))

    (t/is (= nil
             (sut/get-prev-unit {:year 2022 :month :jun :day 4 :hour 12 :min 30}
                                :ms)))

    (t/is (= :year
             (sut/get-next-unit {:min 30}
                                :month)))

    (t/is (= :day
             (sut/get-prev-unit {:min 30}
                                :month))))

  (t/testing "get-unit-seq"
    (t/is (= [:jan :feb  :mar :apr :may  :jun :jul :aug  :sep :oct :nov  :dec]
             (:sequence (sut/get-unit-seq {:year 2022 :month :jun :day 4 :hour 12 :min 30}
                                          :month))))

    (t/is (= [:jan :feb  :mar :apr :may  :jun :jul :aug  :sep :oct :nov  :dec]
             (:sequence (sut/get-unit-seq {:min 30}
                                          :month))))

    (t/is (= (:sequence #unit-map/seq[##-Inf .. -2 -1 1 2 .. ##Inf])
             (:sequence (sut/get-unit-seq {:year 2022 :month :jun :day 4 :hour 12 :min 30}
                                          :year)))))

  (t/testing "get-next-unit-value"
    (t/is (= (range 60)
             (->> (iterate #(sut/get-next-unit-value
                              (get-in @sut/ctx [:seqs :sec :min])
                              nil
                              %)
                           0)
                  (take-while some?))))

    (t/is (= [:jan :feb  :mar :apr :may  :jun :jul :aug  :sep :oct :nov  :dec]
             (->> (iterate #(sut/get-next-unit-value
                              (get-in @sut/ctx [:seqs :month :year])
                              nil
                              %)
                           :jan)
                  (take-while some?))))

    (t/is (= (range 1970 2021)
             (->> (iterate #(sut/get-next-unit-value
                              (get-in @sut/ctx [:seqs :year nil])
                              nil
                              %)
                           1970)
                  (take 51))))

    (t/is (= [12 1 2 3 4 5 6 7 8 9 10 11]
             (->> (iterate #(sut/get-next-unit-value
                              (get-in @sut/ctx [:seqs :am-pm/hour :am-pm/period])
                              nil
                              %)
                           12)
                  (take-while some?))))

    (t/is (= 13 (sut/get-next-unit-value #unit-map/seq[1 3 .. :TODO/remove (fn [{:keys [bar]}] (if (odd? bar) 9 11)) 13 15]
                                         {:bar 7}
                                         9)))

    (t/is (= 11 (sut/get-next-unit-value #unit-map/seq[1 3 .. :TODO/remove (fn [{:keys [bar]}] (if (odd? bar) 9 11)) 13 15]
                                         {:bar 8}
                                         9))))

  (t/testing "get-prev-unit-value"
    (t/is (= (reverse (range 60))
             (->> (iterate #(sut/get-prev-unit-value
                              (get-in @sut/ctx [:seqs :sec :min])
                              nil
                              %)
                           59)
                  (take-while some?))))

    (t/is (= (reverse [:jan :feb  :mar :apr :may  :jun :jul :aug  :sep :oct :nov  :dec])
             (->> (iterate #(sut/get-prev-unit-value
                              (get-in @sut/ctx [:seqs :month :year])
                              nil
                              %)
                           :dec)
                  (take-while some?))))

    (t/is (= (reverse (range 1970 2021))
             (->> (iterate #(sut/get-prev-unit-value
                              (get-in @sut/ctx [:seqs :year nil])
                              nil
                              %)
                           2020)
                  (take 51))))

    (t/is (= (reverse [12 1 2 3 4 5 6 7 8 9 10 11])
             (->> (iterate #(sut/get-prev-unit-value
                              (get-in @sut/ctx [:seqs :am-pm/hour :am-pm/period])
                              nil
                              %)
                           11)
                  (take-while some?))))

    (t/is (= 9 (sut/get-prev-unit-value #unit-map/seq[1 3 .. :TODO/remove (fn [{:keys [bar]}] (if (odd? bar) 9 11)) 13 15]
                                        {:bar 7}
                                        13)))

    (t/is (= 11 (sut/get-prev-unit-value #unit-map/seq[1 3 .. :TODO/remove (fn [{:keys [bar]}] (if (odd? bar) 9 11)) 13 15]
                                         {:bar 8}
                                         13))))

  (t/testing "get first/last el"
    (t/is (= 0 (sut/get-first-el #unit-map/seq[0 1 3 .. :TODO/remove (fn [{:keys [bar]}] (if (odd? bar) 9 11)) 13 15]
                                 {})))

    (t/is (= 15 (sut/get-last-el #unit-map/seq[0 1 3 .. :TODO/remove (fn [{:keys [bar]}] (if (odd? bar) 9 11)) 13 15]
                                 {})))

    (t/is (= 1 (sut/get-first-el #unit-map/seq[(constantly 1) (constantly 1) .. :TODO/remove (constantly 10)]
                                 {})))

    (t/is (= 10 (sut/get-last-el #unit-map/seq[(constantly 1) (constantly 1) .. :TODO/remove (constantly 10)]
                                 {}))))

  (t/testing "get min/max value"
    (t/is (= ##-Inf (sut/get-min-value {:year 2022} :year)))

    (t/is (= ##Inf (sut/get-max-value {:year 2022} :year)))))


(t/deftest inc-dec-test
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
               (take 24 (iterate (partial sut/inc-unit :am-pm/hour) value)))))

    (t/testing "calendar"
      (def value {:day 1, :month :jan, :year 2020})

      (def calendar (->> value
                         (iterate (partial sut/inc-unit :day))
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
               (take 24 (iterate (partial sut/dec-unit :am-pm/hour) value)))))

    (t/testing "calendar"
      (def value {:day 31, :month :dec, :year 2019})

      (def calendar (->> value
                         (iterate (partial sut/dec-unit :day))
                         (take-while (comp #{2019} :year))
                         (partition-by :month)))

      (t/is (= 12 (count calendar)))
      (t/is (= 365 (count (flatten calendar))))))

  (t/testing "add-to-unit"
    (t/is (= {:hour 0, :day 22, :month :aug, :year 2044}
             (sut/add-to-unit {:hour 0 :day 1, :month :jan, :year 2020} :hour 216000)))
    (t/is (= {:hour 0 ,:year 1995, :month :may, :day 12}
             (sut/add-to-unit {:hour 0 :day 1, :month :jan, :year 2020} :hour -216000)))
    (t/is (= {:day 1, :month :jan, :year 2020}
             (sut/add-to-unit {:day 1, :month :jan, :year 2020} :hour 0))))

  (t/testing "subtract-from-unit"
    (t/is (= {:hour 0, :day 22, :month :aug, :year 2044}
             (sut/subtract-from-unit {:day 1, :month :jan, :year 2020} :hour -216000)))
    (t/is (= {:hour 0,:year 1995, :month :may, :day 12}
             (sut/subtract-from-unit {:day 1, :month :jan, :year 2020} :hour 216000)))
    (t/is (= {:day 1, :month :jan, :year 2020}
             (sut/subtract-from-unit {:day 1, :month :jan, :year 2020} :hour 0)))))


(t/deftest cmp
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
             (sut/add-delta t {:ms 200})))

    (t/is (= (merge t {:ms 100, :sec 31})
             (sut/add-delta t {:ms 600})))

    (t/is (= {:ms 1500}
             (sut/add-delta {:ms 600} {:ms 600} {:ms 300})))

    (t/is (= {:ms 500, :sec 1}
             (sut/add-delta {:sec 0, :ms 600} {:ms 600} {:ms 300})))

    (t/is (= (merge t {:sec 50})
             (sut/add-delta t {:sec 20})))

    (t/is (= (merge t {:sec 50})
             (sut/add-delta t {:sec 20})))

    (t/is (= (merge t {:hour 12, :min 50})
             (sut/add-delta t {:min 20})))

    (t/is (= (merge t {:hour 13 :min 0})
             (sut/add-delta t {:min 30})))

    (t/is (= {:year 2019 :month :jan :day 1}
             (sut/add-delta {:year 2018 :month :dec :day 31} {:day 1})))

    (t/is (= {:year 2018 :month :feb :day 1}
             (sut/add-delta {:year 2018 :month :jan :day 1} {:day 31})))

    (t/is (= {:year 2020 :month :jan :day 1}
             (sut/add-delta {:year 2018 :month :dec :day 31} {:day 366})))

    (t/is (= {:year 2018 :month :mar :day 1}
             (sut/add-delta {:year 2018 :month :feb :day 28} {:day 1})))

    (t/is (= {:year 2018 :month :mar :day 31}
             (sut/add-delta {:year 2018 :month :mar :day 30} {:day 1})))

    (t/is (= {:year 2018 :month :apr :day 1}
             (sut/add-delta {:year 2018 :month :mar :day 31} {:day 1})))

    (t/is (= {:ms 400}
             (sut/add-delta {:ms 100} {:ms 300})))

    (t/is (= {:ms 200 :sec 1}
             (sut/add-delta {:sec 0, :ms 900} {:ms 300})))

    (t/is (= {:sec 30 :min 1}
             (sut/add-delta {:min 0, :sec 40} {:sec 50})))

    (t/is (= {:min 30 :hour 1}
             (sut/add-delta {:min 40} {:min 50})))

    (t/is (= {:hour 3 :day 1}
             (sut/add-delta {:day 0, :hour 13} {:hour 14})))

    (t/is (= {:year 2011 :month :jan :day 2 :hour 4}
             (sut/add-delta {:year 2011 :month :jan :day 1 :hour 23} {:hour 5})))

    (t/is (= {:year 2011 :month :feb :day 2}
             (sut/add-delta {:year 2011 :month :jan :day 30} {:day 3})))

    (t/is (= {:year 2012 :month :jan :day 1}
             (sut/add-delta {:year 2011 :month :jan :day 1} {:day 365})))

    (t/is (= {:year 2012 :month :jan :day 1 :hour 4}
             (sut/add-delta {:year 2011 :month :dec :day 31 :hour 23} {:hour 5})))

    (t/is (= {:year 2010 :month :dec :day 31 :hour 23}
             (sut/add-delta {:year 2011 :month :jan :day 1 :hour 0} {:hour -1})))

    (t/is (= {:year 2010 :month :dec :day 31 :hour 23 :min 59 :sec 59}
             (sut/add-delta {:year 2011 :month :jan :day 1 :hour 0} {:sec -1})))

    (t/is (= {:year 2010 :month :dec :day 31 :hour 23 :min 59 :sec 59 :ms 999}
             (sut/add-delta {:year 2011 :month :jan :day 1 :hour 0} {:ms -1})))

    (t/is (= {:year 2010 :month :dec :day 31 :hour 23 :min 30}
             (sut/add-delta {:year 2011 :month :jan :day 1 :hour 23} {:hour -23 :min -30})))

    (t/is (= {:year 2019 :month :dec :day 1}
             (sut/add-delta {:year 2019 :month :nov :day 1} {:month 1})))

    (t/is (= {:year 2020 :month :jan :day 1}
             (sut/add-delta {:year 2019 :month :nov :day 1} {:month 2})))

    (t/is (= {:year 2020 :month :jan :day 1}
             (sut/add-delta {:year 2019 :month :dec :day 1} {:month 1})))

    (t/is (= {:year 2019 :month :dec :day 31}
             (sut/add-delta {:year 2019 :month :nov :day 31} {:month 1})))

    (t/is (= {:year 2020 :month :feb}
             (sut/add-delta {:year 2020 :month :feb} {:day 0})))

    (t/is (= {:year 2019, :month :dec, :day 10, :hour 15, :min 17, :sec 50, :ms 911}
             (sut/add-delta {:year 2019, :month :dec, :day 10, :hour 13, :min 17, :sec 50, :ms 911} {:hour 2})))

    (t/is (= {:hour 14 :tz {:hour 2}}
             (sut/add-delta {:hour 4 :tz {:hour 2}} {:hour 10})))

    (t/is (= {:hour 2 :tz {:hour -2}}
             (sut/add-delta {:hour 1 :tz {:hour -2}} {:hour 1}))))

  (t/testing "-"
    (t/is (= {:year 2016, :month :jan, :day 1, :hour 23, :min 30}
             (sut/subtract-delta {:year 2016, :month :dec, :day 31, :hour 23, :min 30} {:day 365})))

    (t/is (= {:year 2015, :month :dec, :day 31, :hour 23, :min 30}
             (sut/subtract-delta {:year 2016 :month :dec :day 31 :hour 23 :min 30} {:day 366})))

    (t/is (= {:year 2020 :month :jan :day 31}
             (sut/subtract-delta {:year 2020 :month :feb}
                                 {:day 1})))
    (t/is (= {:year 2020 :month :feb}
             (sut/subtract-delta {:year 2020 :month :feb}
                                 {:day 0})))

    (t/is (sut/eq? {:hour 0, :tz {:hour -2}}
                   (sut/subtract-delta {:hour 2 :tz {:hour -2}} {:hour 2})))
    (t/is (sut/eq? {:hour 0}
                   (sut/subtract-delta {:hour 2 :tz {:hour -2}} {:hour 2})))
    (t/is (sut/eq? {:hour 2}
                   (sut/subtract-delta {:hour 3 :tz {:hour 2}} {:hour 1 :tz {:hour 2}})))))


(t/deftest ^:kaocha/pending demo-test
  (sut/defseq :ms   #unit-map/seq[0 1 .. 999 -> :sec])
  (sut/defseq :sec  #unit-map/seq[0 1 .. 59 -> :min])
  (sut/defseq :min  #unit-map/seq[0 1 .. 59 -> :hour])
  (sut/defseq :hour #unit-map/seq[0 1 .. 23 -> :day])

  (sut/defseq :day   #unit-map/seq[1 2 .. days-in-month -> :month])
  (sut/defseq :month #unit-map/seq[:jan :feb  :mar :apr :may  :jun :jul :aug  :sep :oct :nov  :dec -> :year])
  (sut/defseq :year  #unit-map/seq[##-Inf .. -2 -1 1 2 .. ##Inf])

  (sut/defsys ms-year    [:ms :sec :min :hour :day :month :year])

  #_(sut/deffmt :iso/month [:month (fn [v fmt-el] '???)])
  #_(sut/deffmt :iso/day [:day 2 "0"])

  #_"NOTE: arithmetics for now can be stubbed with simple update/inc etc"
  #_"NOTE: need some configs to map months enum to numbers"
  #_"NOTE: for sequences consisting of only static ranges calculate leading 0 padding automatically"

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
