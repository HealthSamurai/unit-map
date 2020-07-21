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
                     {:start 1, :step 1, :end ##Inf}]))))