(ns chrono.new-ops-test
  (:require [chrono.new-ops :as sut]
            [matcho.core :as matcho]
            [clojure.test :as t]))

(t/deftest range-test
  (let [base60 [0 1 '.. 59]
        months [:jan :feb :mar :apr :may :jun :jul :aug :sep :oct :nov :dec]
        years  [##-Inf '.. -2 -1 1 2 '.. ##Inf]]

    (t/testing "process-sequence"
      (matcho/match (sut/process-sequence base60)
                    [{:start 0, :step 1, :end 59}])
      (matcho/match (sut/process-sequence months)
                    [:jan :feb :mar :apr :may :jun :jul :aug :sep :oct :nov :dec])
      (matcho/match (sut/process-sequence years)
                    [{:start ##-Inf, :step 1, :end -1}
                     {:start 1, :step 1, :end ##Inf}]))

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
                    (range 1970 2021)))

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
                    (range 1970 1920)))))
