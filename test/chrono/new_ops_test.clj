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

  (t/testing "sequence-contains?"
    (matcho/match (map (partial sut/sequence-contains? base60)
                       [##-Inf -1 0 59 60 ##Inf])
                  [false false true true false false])
    (matcho/match (map (partial sut/sequence-contains? years)
                       [##-Inf -31337 -1 0 1 31337 ##Inf])
                  [true true true false true true true]))

  (t/testing "get-next"
    (matcho/match (take-while some? (iterate (partial sut/get-next base60) 0))
                  (range 60))

    (matcho/match (take-while some? (iterate (partial sut/get-next months) :jan))
                  [:jan :feb :mar :apr :may :jun :jul :aug :sep :oct :nov :dec])

    (matcho/match (take 51 (iterate (partial sut/get-next years) 1970))
                  (range 1970 2021))

    (matcho/match (take-while some? (iterate (partial sut/get-next am-hours) 12))
                  [12 1 2 3 4 5 6 7 8 9 10 11]))

  (t/testing "get-prev"
    (matcho/match (take-while some? (iterate (partial sut/get-prev base60) 59))
                  (range 59 -1 -1))

    (matcho/match (take-while some? (iterate (partial sut/get-prev months) :dec))
                  [:dec :nov :oct :sep :aug :jul :jun :may :apr :mar :feb :jan])

    (matcho/match (take 51 (iterate (partial sut/get-prev years) 1970))
                  (range 1970 1920))

    (matcho/match (take-while some? (iterate (partial sut/get-prev am-hours) 11))
                  [11 10 9 8 7 6 5 4 3 2 1 12])))
