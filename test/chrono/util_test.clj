(ns chrono.util-test
  (:require [clojure.test :as t]
            [chrono.util :as sut]))


(t/deftest pad-zero-test
  (t/is (= "00321" (sut/pad-zero 5 "321")))
  (t/is (= "1"     (sut/pad-zero 1 "321")))
  (t/is (= "321"   (sut/pad-zero 3 "321"))))
