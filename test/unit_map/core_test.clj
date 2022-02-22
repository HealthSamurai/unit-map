(ns unit-map.core-test
  (:require [unit-map.core :as sut]
            [clojure.test :as t]
            [matcho.core :as matcho]))


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

  (sut/defseq* tctx :b3 #unit-map/seq[:b2 <=> 2 1 0 -> :c3])
  (sut/defseq* tctx :c3 #unit-map/seq[2 1 .. ##-Inf])

  (sut/defseq* tctx :b4 #unit-map/seq[:b <=> 2 1 0 -> :c4])
  (sut/defseq* tctx :c4 #unit-map/seq[2 1 .. ##-Inf])

  (sut/defseq* tctx :b5 #unit-map/seq[2 1 0 -> :c5])
  (sut/defseq* tctx :c5 #unit-map/seq[2 1 .. ##-Inf])

  (sut/defseq* tctx :b6 #unit-map/seq[:b <=> 2 1 0 -> :c6])
  (sut/defseq* tctx :c6 #unit-map/seq[:c <=> 2 1 0 -> :d])

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
       :c2 {:d {:sequence [-2 -1 0], :unit :c2, :next-unit :d}}

       :b3 {:c3 {:sequence [2 1 0], :unit :b3, :next-unit :c3}}
       :c3 {nil {:sequence [{:start 2, :step -1, :end ##-Inf}], :unit :c3}}

       :b4 {:c4 {:sequence [2 1 0], :unit :b4, :next-unit :c4}}
       :c4 {nil {:sequence [{:start 2, :step -1, :end ##-Inf}], :unit :c4}}

       :b5 {:c5 {:sequence [2 1 0], :unit :b5, :next-unit :c5}}
       :c5 {nil {:sequence [{:start 2, :step -1, :end ##-Inf}], :unit :c5}}

       :b6 {:c6 {:sequence [2 1 0], :unit :b6, :next-unit :c6}
            :c  {:sequence [2 1 0], :unit :b6, :next-unit :c}}
       :c6 {:d {:sequence [2 1 0], :unit :c6, :next-unit :d}}})

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
    (t/is (not (sut/sys-continuous?* tctx [:a :b3 :c3 :d])))))


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
  (matcho/match
    (sut/guess-sys {:min 30, :hour 15})
    [ms-hour])

  (matcho/match
    (sut/guess-sys {:min 30, :am-pm/hour 3, :am-pm/period :pm})
    [ms-day-am-pm])

  (matcho/match
    (sut/guess-sys {:ns 1, :ms 1, :sec 1, :min 1, :hour 1, :day 1})
    [ns-ms-day])

  (matcho/match
    (sut/guess-sys {:ns 1, :ms 1, :sec 1, :min 1, :hour 25})
    [ns-ms-hour])

  (matcho/match
    (sut/guess-sys {:ns 1, :ms 1, :sec 1, :min 1501})
    [ns-ms-hour])

  (matcho/match
    (sut/guess-sys {:ns 1, :ms 1, :sec 90061})
    [seconds])

  (matcho/match
    (sut/guess-sys {:ns 1, :ms 90061001})
    [seconds])

  (matcho/match
    (sut/guess-sys {:ns 90061001000001})
    [ns-timestamp])


  (matcho/match
    (sut/guess-sys {:ns 1000001, :sec 1, :min 1, :hour 1, :day 1})
    [ns-day])

  (matcho/match
    (sut/guess-sys {:ns 1000001, :sec 1, :min 1, :hour 25})
    [ns-hour])

  (matcho/match
    (sut/guess-sys {:ns 1000001, :sec 1, :min 1501})
    [ns-hour])

  (matcho/match
    (sut/guess-sys {:ns 1000001, :sec 90061})
    [ns-seconds])

  (matcho/match
    (sut/guess-sys {:ns 90061001000001})
    [ns-timestamp])

  (matcho/match
    (sut/guess-sys {:ns 1, :sec 1, :min 1, :hour 1, :day 1 :delta {:ns 1}})
    [ns-day])


  (matcho/match
    (sut/guess-sys {:ms 1, :sec 1, :min 1, :hour 1, :day 1})
    [ms-day])

  (matcho/match
    (sut/guess-sys {:ms 1, :sec 1, :min 1, :hour 25})
    [ms-hour])

  (matcho/match
    (sut/guess-sys {:ms 1, :sec 1, :min 1501})
    [ms-hour])

  (matcho/match
    (sut/guess-sys {:ms 1, :sec 90061})
    [seconds])

  (matcho/match
    (sut/guess-sys {:ms 90061001})
    [timestamp])

  (matcho/match
    (sut/guess-sys {:ms 1, :sec 1, :min 1, :hour 1, :day 1 :delta {:ms 1}})
    [ms-day])

  (matcho/match
    (sut/guess-sys {})
    empty?)

  (matcho/match
    (sut/guess-sys nil)
    empty?))


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
    (matcho/match
      (sut/sys-intersection
        {:year 2021, :month :sep, :day 7, :hour 21, :min 30, :tz {:hour 2}}
        {:delta {:hour 3}})
      [ms-year ns-year ns-ms-year nil])

    (matcho/match
      (sut/sys-intersection
        {:year 2021, :month :sep, :day 7, :hour 21, :min 30, :tz {:hour 2}}
        {:year 2021, :month :sep, :day 7, :hour 22, :min 30, :tz {:hour 3}})
      [ms-year ns-year ns-ms-year nil])

    (matcho/match
      (sut/sys-intersection
        {:year 2021, :month :sep, :day 7, :hour 21, :min 30, :tz {:hour 2}}
        {:year 2021})
      [ms-year ns-year ns-ms-year nil])

    (matcho/match
      (sut/sys-intersection
        {:year 2021, :month :sep, :day 7, :hour 21, :min 30, :tz {:hour 2}}
        {:cm 49})
      empty?)

    (matcho/match
      (sut/sys-intersection
        {:year 2021, :month :sep, :day 7, :hour 21, :min 30, :tz {:hour 2}}
        {})
      empty?)

    (matcho/match
      (sut/sys-intersection
        {:year 2021, :month :sep, :day 7, :hour 21, :min 30, :tz {:hour 2}})
      [ms-year ns-year ns-ms-year nil]))

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
             (sut/concretize-range (-> #unit-map/seq[1 2 .. (fn [{:keys [month]}] (if (= 2 month) 28 30))]
                                       :sequence
                                       first)
                                   {:day 1, :month 2, :year 2022}))))

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

    (t/is (= ##Inf (sut/sequence-last-index #unit-map/seq[0 1 .. ##Inf]
                                            nil)))

    (t/is (= ##Inf #_"TODO: probably should be 0"
             (sut/sequence-last-index
               #unit-map/seq[##-Inf .. -1 0]
               nil)))

    (t/is (= ##Inf (sut/sequence-last-index #unit-map/seq[##-Inf .. -1 0 1 2 .. ##Inf]
                                            nil))))

  (t/testing "contains"
    (t/is (some? (sut/sequence-contains-some
                   #unit-map/seq[##-Inf .. -2 -1 1 2 .. ##Inf]
                   nil
                   10)))

    (t/is (some? (sut/sequence-contains-some
                   #unit-map/seq[##-Inf .. -2 -1 1 2 .. ##Inf]
                   nil
                   -10)))

    (t/is (some? (sut/sequence-contains-some
                   #unit-map/seq[##-Inf .. -2 -1 1 2 .. ##Inf]
                   nil
                   ##Inf)))

    (t/is (some? (sut/sequence-contains-some
                   #unit-map/seq[##-Inf .. -2 -1 1 2 .. ##Inf]
                   nil
                   ##-Inf)))

    (t/is (nil? (sut/sequence-contains-some
                  #unit-map/seq[##-Inf .. -2 -1 1 2 .. ##Inf]
                  nil
                  0)))))
