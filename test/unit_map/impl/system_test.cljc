(ns unit-map.impl.system-test
  (:require [unit-map.impl.system :as sut]
            [unit-map.core :as umap]
            [clojure.test :as t]))


(def registry-atom (atom nil))


(do ;;NOTE: seqs
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
       ::m  -3
       :μ  -6
       :n  -9
       :p  -12
       :f  -15
       :a  -18
       :z  -21
       :y  -24})


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


  (defn weekday [{::keys [weekday]}]
    (condp contains? weekday
      #{:mon :tue :wed :thu :fri} :workday
      #{:sat :sun}                :weekend))


  (defn season [{::keys [month]}]
    (condp contains? month
      #{:dec :jan :feb} :winter
      #{:mar :apr :may} :spring
      #{:jun :jul :aug} :summer
      #{:sep :oct :nov} :autumn))

  (umap/defseq registry-atom ::ns   #unit-map/seq[0 1 .. 999999999 -> ::sec])

  (umap/defseq registry-atom ::ns   #unit-map/seq[0 1 .. 999999 -> ::ms])
  (umap/defseq registry-atom ::ms   #unit-map/seq[0 1 .. 999 -> ::sec])
  (umap/defseq registry-atom ::sec  #unit-map/seq[0 1 .. 59 -> ::min])
  (umap/defseq registry-atom ::min  #unit-map/seq[0 1 .. 59 -> ::hour])
  (umap/defseq registry-atom ::hour #unit-map/seq[0 1 .. 23 -> ::day])

  (umap/defseq registry-atom ::ms   #unit-map/seq[0 1 .. ##Inf])
  (umap/defseq registry-atom ::ns   #unit-map/seq[0 1 .. ##Inf])
  (umap/defseq registry-atom ::sec  #unit-map/seq[0 1 .. ##Inf])
  (umap/defseq registry-atom ::hour #unit-map/seq[0 1 .. ##Inf])
  (umap/defseq registry-atom ::day  #unit-map/seq[0 1 .. ##Inf]) #_"NOTE: should start with 0 or with 1?"

  (umap/defseq registry-atom ::ampm-hour   #unit-map/seq[::hour <=> 12 1 2 .. 11 -> ::ampm-period])
  (umap/defseq registry-atom ::ampm-period #unit-map/seq[:am :pm -> ::day])

  (umap/defseq registry-atom ::day   #unit-map/seq[1 2 .. days-in-month -> ::month])
  (umap/defseq registry-atom ::month #unit-map/seq[:jan :feb  :mar :apr :may  :jun :jul :aug  :sep :oct :nov  :dec -> ::year])
  (umap/defseq registry-atom ::year  #unit-map/seq[##-Inf .. -2 -1 1 2 .. ##Inf])

  (umap/defseq registry-atom ::weekday  #unit-map/seq[::day <=> :mon :tue :wed :thu :fri :sat :sun -> ::week])
  (umap/defseq registry-atom ::week     #unit-map/seq[1 2 .. 52])
  (umap/defseq registry-atom ::weekpart #unit-map/seq[::weekday <=> weekday])
  (umap/defseq registry-atom ::season   #unit-map/seq[::month <=> season])

  (umap/defseq registry-atom ::mil  #unit-map/seq[0 1 .. 999  -> ::inch])
  (umap/defseq registry-atom ::inch #unit-map/seq[0 1 .. 11   -> ::foot])
  (umap/defseq registry-atom ::foot #unit-map/seq[0 1 .. 5279 -> ::mile])
  (umap/defseq registry-atom ::mile #unit-map/seq[0 1 .. ##Inf])

  (umap/defseq registry-atom ::mm #unit-map/seq[0 1 .. 9   -> ::cm])
  (umap/defseq registry-atom ::cm #unit-map/seq[0 1 .. 99  -> ::m])
  (umap/defseq registry-atom ::m  #unit-map/seq[0 1 .. 999 -> ::km])
  (umap/defseq registry-atom ::km #unit-map/seq[0 1 .. ##Inf])

  (umap/defseq registry-atom ::epoch-year  #unit-map/seq[::year <=>
                                                        (fn [{::keys [epoch]}]
                                                          (if (= :BC epoch) ##Inf 1))
                                                        (fn [{::keys [epoch]}]
                                                          (if (= :BC epoch) -1 1))
                                                        ..
                                                        (fn [{::keys [epoch]}]
                                                          (if (= :BC epoch) 1 ##Inf))
                                                        -> ::epoch])

  (umap/defseq registry-atom ::epoch  #unit-map/seq[:BC :AD]))

(do ;;NOTE: systems
  (umap/defsys registry-atom 'imperial [::mil ::inch ::foot ::mile])

  (umap/defsys registry-atom 'metric   [::mm ::cm ::m ::km])

  (umap/defsys registry-atom 'ms-hour    [::ms ::sec ::min ::hour])
  (umap/defsys registry-atom 'ns-hour    [::ns ::sec ::min ::hour])
  (umap/defsys registry-atom 'ns-ms-hour [::ns ::ms ::sec ::min ::hour])

  (umap/defsys registry-atom 'timestamp    [::ms])
  (umap/defsys registry-atom 'ns-timestamp [::ns])

  (umap/defsys registry-atom 'seconds [::ns ::ms ::sec])
  (umap/defsys registry-atom 'ns-seconds [::ns ::sec])

  (umap/defsys registry-atom 'ms-day    [::ms ::sec ::min ::hour ::day])
  (umap/defsys registry-atom 'ns-day    [::ns ::sec ::min ::hour ::day])
  (umap/defsys registry-atom 'ns-ms-day [::ns ::ms ::sec ::min ::hour ::day])

  (umap/defsys registry-atom 'ms-day-am-pm    [::ms ::sec ::min ::ampm-hour ::ampm-period ::day])
  (umap/defsys registry-atom 'ns-day-am-pm    [::ns ::sec ::min ::ampm-hour ::ampm-period ::day])
  (umap/defsys registry-atom 'ns-ms-day-am-pm [::ns ::ms ::sec ::min ::ampm-hour ::ampm-period ::day])

  (umap/defsys registry-atom 'date       [::day ::month ::year])
  (umap/defsys registry-atom 'month-year [::month ::year])

  (umap/defsys registry-atom 'ms-year    [::ms ::sec ::min ::hour ::day ::month ::year])
  (umap/defsys registry-atom 'ns-year    [::ns ::sec ::min ::hour ::day ::month ::year])
  (umap/defsys registry-atom 'ns-ms-year [::ns ::ms ::sec ::min ::hour ::day ::month ::year])

  (umap/defsys registry-atom 'ms-year-am-pm    [::ms ::sec ::min ::ampm-hour ::ampm-period ::day ::month ::year])
  (umap/defsys registry-atom 'ns-year-am-pm    [::ns ::sec ::min ::ampm-hour ::ampm-period ::day ::month ::year])
  (umap/defsys registry-atom 'ns-ms-year-am-pm [::ns ::ms ::sec ::min ::ampm-hour ::ampm-period ::day ::month ::year])

  (umap/defsys registry-atom 'weeks [::weekday ::week])

  (umap/defsys registry-atom 'ms-year-epoch [::ms ::sec ::min ::hour ::day ::month ::epoch-year ::epoch])
  (umap/defsys registry-atom 'year-epoch [::epoch-year ::epoch])

  (->> (for [[sys sys-def] (:systems @registry-atom)
             :when (symbol? sys)]
         (list 'def sys sys-def))
       (cons 'do)
       eval #_"TODO: refactor this"))


(t/deftest sys-detection
  (t/is (= ms-hour
           (sut/guess-sys @registry-atom {::min 30, ::hour 15})))

  (t/is (= ms-day-am-pm
           (sut/guess-sys @registry-atom {::min 30, ::ampm-hour 3, ::ampm-period :pm})))

  (t/is (= ns-ms-day
           (sut/guess-sys @registry-atom {::ns 1, ::ms 1, ::sec 1, ::min 1, ::hour 1, ::day 1})))

  (t/is (= ns-ms-hour
           (sut/guess-sys @registry-atom {::ns 1, ::ms 1, ::sec 1, ::min 1, ::hour 25})))

  (t/is (= ns-ms-hour
           (sut/guess-sys @registry-atom {::ns 1, ::ms 1, ::sec 1, ::min 1501})))

  (t/is (= seconds
           (sut/guess-sys @registry-atom {::ns 1, ::ms 1, ::sec 90061})))

  (t/is (= seconds
           (sut/guess-sys @registry-atom {::ns 1, ::ms 90061001})))

  (t/is (= ns-timestamp
           (sut/guess-sys @registry-atom {::ns 90061001000001})))


  (t/is (= ns-day
           (sut/guess-sys @registry-atom {::ns 1000001, ::sec 1, ::min 1, ::hour 1, ::day 1})))

  (t/is (= ns-hour
           (sut/guess-sys @registry-atom {::ns 1000001, ::sec 1, ::min 1, ::hour 25})))

  (t/is (= ns-hour
           (sut/guess-sys @registry-atom {::ns 1000001, ::sec 1, ::min 1501})))

  (t/is (= ns-seconds
           (sut/guess-sys @registry-atom {::ns 1000001, ::sec 90061})))

  (t/is (= ns-timestamp
           (sut/guess-sys @registry-atom {::ns 90061001000001})))

  (t/is (= ns-day
           (sut/guess-sys @registry-atom {::ns 1, ::sec 1, ::min 1, ::hour 1, ::day 1 :delta {::ns 1}})))


  (t/is (= ms-day
           (sut/guess-sys @registry-atom {::ms 1, ::sec 1, ::min 1, ::hour 1, ::day 1})))

  (t/is (= ms-hour
           (sut/guess-sys @registry-atom {::ms 1, ::sec 1, ::min 1, ::hour 25})))

  (t/is (= ms-hour
           (sut/guess-sys @registry-atom {::ms 1, ::sec 1, ::min 1501})))

  (t/is (= ms-day
           (sut/guess-sys @registry-atom {::ms 1, ::sec 1, ::min 1501}
                          ::day)))

  (t/is (= seconds
           (sut/guess-sys @registry-atom {::ms 1, ::sec 90061})))

  (t/is (= timestamp
           (sut/guess-sys @registry-atom {::ms 90061001})))

  (t/is (= ms-day
           (sut/guess-sys @registry-atom {::ms 1, ::sec 1, ::min 1, ::hour 1, ::day 1 :delta {::ms 1}})))

  (t/is (nil? (sut/guess-sys @registry-atom {})))

  (t/is (nil? (sut/guess-sys @registry-atom nil))))


(t/deftest find-diff-branches-unit-test
  (sut/find-diff-branches [::ns ::ms ::sec ::min ::ampm-hour ::ampm-period ::day ::month ::year]
                          [::ns ::sec ::min ::hour :period ::day ::month ::year])
  ;; => [::ns
  ;;     [[::ms] []]
  ;;     ::sec
  ;;     ::min
  ;;     [[::ampm-hour ::ampm-period] [::hour :period]]
  ;;     ::day
  ;;     ::month
  ;;     ::year]

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

  (t/is (= [[[1 2 3] [::a ::b ::c]]]
           (sut/find-diff-branches [1 2 3] [::a ::b ::c]))))


(t/deftest sys-conversion
  (t/testing "interseciton"
    (t/is (= ms-year
             (sut/sys-intersection @registry-atom
                                   {::year 2021, ::month :sep, ::day 7, ::hour 21, ::min 30, :tz {::hour 2}}
                                   {:delta {::hour 3}})))

    (t/is (= ms-year
             (sut/sys-intersection @registry-atom
                                   {::year 2021, ::month :sep, ::day 7, ::hour 21, ::min 30, :tz {::hour 2}}
                                   {::year 2021, ::month :sep, ::day 7, ::hour 22, ::min 30, :tz {::hour 3}})))

    (t/is (= ms-year
             (sut/sys-intersection @registry-atom
                                   {::year 2021, ::month :sep, ::day 7, ::hour 21, ::min 30, :tz {::hour 2}}
                                   {::year 2021})))

    (t/is (nil? (sut/sys-intersection @registry-atom
                                      {::year 2021, ::month :sep, ::day 7, ::hour 21, ::min 30, :tz {::hour 2}}
                                      {::cm 49})))

    (t/is (= ms-year
             (sut/sys-intersection @registry-atom
                                   {::year 2021, ::month :sep, ::day 7, ::hour 21, ::min 30, :tz {::hour 2}}
                                   {})))

    (t/is (= ms-year
             (sut/sys-intersection @registry-atom
                                   {::year 2021, ::month :sep, ::day 7, ::hour 21, ::min 30, :tz {::hour 2}}))))

  (t/testing "find conversion"
    #_"TODO: timezones"

    (t/is (= [{[::ms]    [::ms]}
              {[::sec]   [::sec]}
              {[::min]   [::min]}
              {[::hour]  [::hour]}
              {[::day]   [::day]}
              {[::month] [::month]}
              {[::year]  [::year]}]
             (sut/find-conversion
               @registry-atom
               {::year 2021, ::month :sep, ::day 7, ::hour 21, ::min 30}
               {::year 2021, ::month :sep, ::day 7, ::hour 21, ::min 30})))

    (t/is (= [{[::ms]    [::ms]}
              {[::sec]   [::sec]}
              {[::min]   [::min]}
              {[::hour]  [::ampm-hour ::ampm-period]}
              {[::day]   [::day]}
              {[::month] [::month]}
              {[::year]  [::year]}]
             (sut/find-conversion
               @registry-atom
               {::year 2021, ::month :sep, ::day 7, ::hour 21, ::min 30}
               {::year 2021, ::month :sep, ::day 7, ::ampm-period :pm, ::ampm-hour 9, ::min 30})))

    (t/testing "different start"
      (t/is (= [{[]       [::ns]}
                {[::ms]    [::ms]}
                {[::sec]   [::sec]}
                {[::min]   [::min]}
                {[::hour]  [::ampm-hour ::ampm-period]}
                {[::day]   [::day]}
                {[::month] [::month]}
                {[::year]  [::year]}]
               (sut/find-conversion
                 @registry-atom
                 {::year 2021, ::month :sep, ::day 7, ::hour 21, ::min 30, ::sec 10, ::ms 10}
                 {::year 2021, ::month :sep, ::day 7, ::ampm-period :pm, ::ampm-hour 9, ::min 30, ::sec 10, ::ms 10, ::ns 10}))))

    (t/testing "two parallel graph paths"
      (t/is (= [{[::ns]    [::ns]}
                {[]       [::ms]}
                {[::sec]   [::sec]}
                {[::min]   [::min]}
                {[::hour]  [::ampm-hour ::ampm-period]}
                {[::day]   [::day]}
                {[::month] [::month]}
                {[::year]  [::year]}]
               (sut/find-conversion
                 @registry-atom
                 {::year 2021, ::month :sep, ::day 7, ::hour 21, ::min 30, ::sec 10, ::ns 10000010}
                 {::year 2021, ::month :sep, ::day 7, ::ampm-period :pm, ::ampm-hour 9, ::min 30, ::sec 10, ::ms 10, ::ns 10}))))

    (t/testing "no conversion"
      (t/is (empty?
              (sut/find-conversion
                @registry-atom
                {::year 2021, ::month :sep, ::day 7, ::hour 21, ::min 30, ::sec 10, ::ns 10000010}
                {::m 1, ::cm 82}))))

    (t/testing "no common units, no common finish"
      (t/is (= [{[::weekday ::week] [::day ::month ::year]}]
               (sut/find-conversion
                 @registry-atom
                 {::week 6}
                 {::year 2022, ::month :jan, ::day 1}))))))


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
             (sut/concretize-range (-> #unit-map/seq[1 2 .. (fn [{::keys [month]}] (if (= :feb month) 28 30))]
                                       :sequence
                                       first)
                                   {::day 1, ::month :feb, ::year 2022}))))

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
    (t/is (= ::month
             (sut/get-next-unit @registry-atom
                                {::year 2022 ::month :jun ::day 4 ::hour 12 ::min 30}
                                ::day)))

    (t/is (= ::hour
             (sut/get-prev-unit @registry-atom
                                {::year 2022 ::month :jun ::day 4 ::hour 12 ::min 30}
                                ::day)))

    (t/is (= nil
             (sut/get-next-unit @registry-atom
                                {::year 2022 ::month :jun ::day 4 ::hour 12 ::min 30}
                                ::year)))

    (t/is (= ::sec
             (sut/get-prev-unit @registry-atom
                                {::year 2022 ::month :jun ::day 4 ::hour 12 ::min 30}
                                ::min)))

    (t/is (= ::ms
             (sut/get-prev-unit @registry-atom
                                {::year 2022 ::month :jun ::day 4 ::hour 12 ::min 30}
                                ::sec)))

    (t/is (= nil
             (sut/get-prev-unit @registry-atom
                                {::year 2022 ::month :jun ::day 4 ::hour 12 ::min 30}
                                ::ms)))

    (t/is (= ::year
             (sut/get-next-unit @registry-atom
                                {::min 30}
                                ::month)))

    (t/is (= ::day
             (sut/get-prev-unit @registry-atom
                                {::min 30}
                                ::month))))

  (t/testing "get-unit-seq"
    (t/is (= [:jan :feb  :mar :apr :may  :jun :jul :aug  :sep :oct :nov  :dec]
             (:sequence (sut/get-unit-seq @registry-atom
                                          {::year 2022 ::month :jun ::day 4 ::hour 12 ::min 30}
                                          ::month))))

    (t/is (= [:jan :feb  :mar :apr :may  :jun :jul :aug  :sep :oct :nov  :dec]
             (:sequence (sut/get-unit-seq @registry-atom
                                          {::min 30}
                                          ::month))))

    (t/is (= (:sequence #unit-map/seq[##-Inf .. -2 -1 1 2 .. ##Inf])
             (:sequence (sut/get-unit-seq @registry-atom
                                          {::year 2022 ::month :jun ::day 4 ::hour 12 ::min 30}
                                          ::year)))))

  (t/testing "get-next-unit-value"
    (t/is (= (range 60)
             (->> (iterate #(sut/get-next-unit-value
                              (get-in @registry-atom [:seqs ::sec ::min])
                              nil
                              %)
                           0)
                  (take-while some?))))

    (t/is (= [:jan :feb  :mar :apr :may  :jun :jul :aug  :sep :oct :nov  :dec]
             (->> (iterate #(sut/get-next-unit-value
                              (get-in @registry-atom [:seqs ::month ::year])
                              nil
                              %)
                           :jan)
                  (take-while some?))))

    (t/is (= (range 1970 2021)
             (->> (iterate #(sut/get-next-unit-value
                              (get-in @registry-atom [:seqs ::year nil])
                              nil
                              %)
                           1970)
                  (take 51))))

    (t/is (= [12 1 2 3 4 5 6 7 8 9 10 11]
             (->> (iterate #(sut/get-next-unit-value
                              (get-in @registry-atom [:seqs ::ampm-hour ::ampm-period])
                              nil
                              %)
                           12)
                  (take-while some?))))

    (t/is (= 13 (sut/get-next-unit-value #unit-map/seq[1 3 .. :TODO/remove (fn [{::keys [bar]}] (if (odd? bar) 9 11)) 13 15]
                                         {::bar 7}
                                         9)))

    (t/is (= 11 (sut/get-next-unit-value #unit-map/seq[1 3 .. :TODO/remove (fn [{::keys [bar]}] (if (odd? bar) 9 11)) 13 15]
                                         {::bar 8}
                                         9))))

  (t/testing "get-prev-unit-value"
    (t/is (= (reverse (range 60))
             (->> (iterate #(sut/get-prev-unit-value
                              (get-in @registry-atom [:seqs ::sec ::min])
                              nil
                              %)
                           59)
                  (take-while some?))))

    (t/is (= (reverse [:jan :feb  :mar :apr :may  :jun :jul :aug  :sep :oct :nov  :dec])
             (->> (iterate #(sut/get-prev-unit-value
                              (get-in @registry-atom [:seqs ::month ::year])
                              nil
                              %)
                           :dec)
                  (take-while some?))))

    (t/is (= (reverse (range 1970 2021))
             (->> (iterate #(sut/get-prev-unit-value
                              (get-in @registry-atom [:seqs ::year nil])
                              nil
                              %)
                           2020)
                  (take 51))))

    (t/is (= (reverse [12 1 2 3 4 5 6 7 8 9 10 11])
             (->> (iterate #(sut/get-prev-unit-value
                              (get-in @registry-atom [:seqs ::ampm-hour ::ampm-period])
                              nil
                              %)
                           11)
                  (take-while some?))))

    (t/is (= 9 (sut/get-prev-unit-value #unit-map/seq[1 3 .. :TODO/remove (fn [{::keys [bar]}] (if (odd? bar) 9 11)) 13 15]
                                        {::bar 7}
                                        13)))

    (t/is (= 11 (sut/get-prev-unit-value #unit-map/seq[1 3 .. :TODO/remove (fn [{::keys [bar]}] (if (odd? bar) 9 11)) 13 15]
                                         {::bar 8}
                                         13))))

  (t/testing "get first/last el"
    (t/is (= 0 (sut/get-first-el #unit-map/seq[0 1 3 .. :TODO/remove (fn [{::keys [bar]}] (if (odd? bar) 9 11)) 13 15]
                                 {})))

    (t/is (= 15 (sut/get-last-el #unit-map/seq[0 1 3 .. :TODO/remove (fn [{::keys [bar]}] (if (odd? bar) 9 11)) 13 15]
                                 {})))

    (t/is (= 1 (sut/get-first-el #unit-map/seq[(constantly 1) (constantly 1) .. :TODO/remove (constantly 10)]
                                 {})))

    (t/is (= 10 (sut/get-last-el #unit-map/seq[(constantly 1) (constantly 1) .. :TODO/remove (constantly 10)]
                                 {}))))

  (t/testing "get min/max value"
    (t/is (= ##-Inf (sut/get-min-value @registry-atom {::year 2022} ::year)))

    (t/is (= ##Inf (sut/get-max-value @registry-atom {::year 2022} ::year)))))
