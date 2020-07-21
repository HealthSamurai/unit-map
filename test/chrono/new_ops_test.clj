(ns chrono.new-ops-test
  (:require [chrono.new-ops :as sut]
            [matcho.core :as matcho]
            [clojure.test :as t]))

(t/deftest range-test
  (def base60   (:min  (sut/rules {})))
  (def months   (:month (sut/rules {})))
  (def years    (:year (sut/rules {})))
  (def am-hours (:hour (sut/rules ^:am-pm{})))

  (t/testing "process-sequence"
    (matcho/match (sut/process-sequence base60)
                  [{:start 0, :step 1, :end 59}])

    (matcho/match (sut/process-sequence months)
                  [:jan :feb :mar :apr :may :jun :jul :aug :sep :oct :nov :dec])

    (matcho/match (sut/process-sequence years)
                  [{:start ##-Inf, :step 1, :end -1}
                   {:start 1, :step 1, :end ##Inf}])

    (matcho/match (sut/process-sequence am-hours)
                  [12 {:start 1, :step 1, :end 11}]))

  (t/testing "range-contains?"
    (matcho/match (map (partial sut/range-contains? (first (sut/process-sequence base60)))
                       [##-Inf -1 0 59 60 ##Inf])
                  [false false true true false false])
    (matcho/match (map (partial sut/range-contains? (first (sut/process-sequence years)))
                       [##-Inf -31337 -1 0 1])
                  [true true true false false])
    (matcho/match (map (partial sut/range-contains? (last (sut/process-sequence years)))
                       [##Inf 31337 1 0 -1])
                  [true true true false false]))

  (t/testing "get-next"
    (matcho/match (->> 0
                       (iterate (partial sut/get-next (sut/process-sequence base60)))
                       (take-while some?))
                  (range 60))

    (matcho/match (->> :jan
                       (iterate (partial sut/get-next (sut/process-sequence months)))
                       (take-while some?))
                  [:jan :feb :mar :apr :may :jun :jul :aug :sep :oct :nov :dec])

    (matcho/match (->> 1970
                       (iterate (partial sut/get-next (sut/process-sequence years)))
                       (take 51))
                  (range 1970 2021))

    (matcho/match (->> 12
                       (iterate (partial sut/get-next (sut/process-sequence am-hours)))
                       (take-while some?))
                  [12 1 2 3 4 5 6 7 8 9 10 11]))

  (t/testing "get-prev"
    (matcho/match (->> 59
                       (iterate (partial sut/get-prev (sut/process-sequence base60)))
                       (take-while some?))
                  (range 59 -1 -1))

    (matcho/match (->> :dec
                       (iterate (->> months
                                     sut/process-sequence
                                     (partial sut/get-prev)))
                       (take-while some?))
                  [:dec :nov :oct :sep :aug :jul :jun :may :apr :mar :feb :jan])

    (matcho/match (->> 1970
                       (iterate (partial sut/get-prev (sut/process-sequence years)))
                       (take 51))
                  (range 1970 1920))

    (matcho/match (->> 11
                       (iterate (partial sut/get-prev (sut/process-sequence am-hours)))
                       (take-while some?))
                  [11 10 9 8 7 6 5 4 3 2 1 12])))
