(ns unit-map.impl.io-test
  (:require [unit-map.impl.io :as sut]
            [clojure.test :as t]))


(t/deftest name-test
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


