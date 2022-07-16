(ns unit-map.impl.registry-test
  (:require [unit-map.impl.registry :as sut]
            [clojure.test :as t]))


(t/deftest reguseq-regusys
  (def registry
    (-> {}
        (sut/reg-useq :unit :a, :useq #unit-map/useq[0 1] :next-unit :b)
        (sut/reg-useq :unit :b, :useq #unit-map/useq[0 1] :next-unit :c)
        (sut/reg-useq :unit :c, :useq #unit-map/useq[0 1] :next-unit :d)
        (sut/reg-useq :unit :d, :useq #unit-map/useq[0 1])

        (sut/reg-useq :unit :aa, :useq #unit-map/useq[0 2] :next-unit :b)
        (sut/reg-useq :unit :a, :useq #unit-map/useq[0 1 2 3] :next-unit :c)

        (sut/reg-useq :unit :b2, :useq #unit-map/useq[-2 -1 0] :next-unit :c2, :eq-unit :b)
        (sut/reg-useq :unit :c2, :useq #unit-map/useq[-2 -1 0] :next-unit :d)
        (sut/reg-useq :unit :c2, :useq #unit-map/useq[-2 -1 .. ##-Inf])

        (sut/reg-useq :unit :b3, :useq #unit-map/useq[2 1 0] :next-unit :c3, :eq-unit :b2)
        (sut/reg-useq :unit :c3, :useq #unit-map/useq[2 1 .. ##-Inf])

        (sut/reg-useq :unit :b4, :useq #unit-map/useq[2 1 0] :next-unit :c4, :eq-unit :b)
        (sut/reg-useq :unit :c4, :useq #unit-map/useq[2 1 .. ##-Inf])

        (sut/reg-useq :unit :b5, :useq #unit-map/useq[2 1 0] :next-unit :c5)
        (sut/reg-useq :unit :c5, :useq #unit-map/useq[2 1 .. ##-Inf])

        (sut/reg-useq :unit :b6, :useq #unit-map/useq[2 1 0] :next-unit :c6, :eq-unit :b)
        (sut/reg-useq :unit :c6, :useq #unit-map/useq[2 1 0] :next-unit :d, :eq-unit :c)
        (sut/reg-useq :unit :c6, :useq #unit-map/useq[2 1 .. ##-Inf])))

  (t/testing "useqs graph"
    (def graph-assert
      {:a {:b  {:useq [0 1], :unit :a, :next-unit :b}
           :b2 {:useq [0 1], :unit :a, :next-unit :b2}
           :b3 {:useq [0 1], :unit :a, :next-unit :b3}
           :b4 {:useq [0 1], :unit :a, :next-unit :b4}
           :b6 {:useq [0 1], :unit :a, :next-unit :b6}
           :c  {:useq [0 1 2 3], :unit :a, :next-unit :c}
           :c6 {:useq [0 1 2 3], :unit :a, :next-unit :c6}}
       :b {:c  {:useq [0 1], :unit :b, :next-unit :c}
           :c6 {:useq [0 1], :unit :b, :next-unit :c6}}
       :c {:d  {:useq [0 1], :unit :c, :next-unit :d}}
       :d {nil {:useq [0 1], :unit :d}}

       :aa {:b  {:useq [0 2], :unit :aa, :next-unit :b}
            :b2 {:useq [0 2], :unit :aa, :next-unit :b2}
            :b3 {:useq [0 2], :unit :aa, :next-unit :b3}
            :b4 {:useq [0 2], :unit :aa, :next-unit :b4}
            :b6 {:useq [0 2], :unit :aa, :next-unit :b6}}

       :b2 {:c2 {:useq [-2 -1 0], :unit :b2, :next-unit :c2}}
       :c2 {:d {:useq [-2 -1 0], :unit :c2, :next-unit :d}
            nil {:useq [{:start -2, :step 1, :end ##-Inf}], :unit :c2}}

       :b3 {:c3 {:useq [2 1 0], :unit :b3, :next-unit :c3}}
       :c3 {nil {:useq [{:start 2, :step -1, :end ##-Inf}], :unit :c3}}

       :b4 {:c4 {:useq [2 1 0], :unit :b4, :next-unit :c4}}
       :c4 {nil {:useq [{:start 2, :step -1, :end ##-Inf}], :unit :c4}}

       :b5 {:c5 {:useq [2 1 0], :unit :b5, :next-unit :c5}}
       :c5 {nil {:useq [{:start 2, :step -1, :end ##-Inf}], :unit :c5}}

       :b6 {:c6 {:useq [2 1 0], :unit :b6, :next-unit :c6}
            :c  {:useq [2 1 0], :unit :b6, :next-unit :c}}
       :c6 {:d {:useq [2 1 0], :unit :c6, :next-unit :d}
            nil {:useq [{:start 2, :step -1, :end ##-Inf}], :unit :c6}}})

    (t/is (= graph-assert (:useqs registry)))

    (t/is (= #{#{:a} #{:aa}
               #{:b :b2 :b3 :b4 :b6} #{:b5}
               #{:c :c6} #{:c2} #{:c3} #{:c4} #{:c5}
               #{:d}}
             (:eq-units registry))))

  (t/testing "valid systems"
    (t/is (sut/usys-continuous? registry [:a :b :c :d]))
    (t/is (sut/usys-continuous? registry [:a :b2 :c2 :d]))
    (t/is (sut/usys-continuous? registry [:a :b2 :c2]))
    (t/is (sut/usys-continuous? registry [:a :b3 :c3]))
    (t/is (sut/usys-continuous? registry [:a :b4 :c4]))
    (t/is (sut/usys-continuous? registry [:b5 :c5]))
    (t/is (sut/usys-continuous? registry [:a :b6 :c6 :d]))
    (t/is (sut/usys-continuous? registry [:a :b6 :c6]))
    (t/is (sut/usys-continuous? registry [:a :b :c6 :d])))

  (t/testing "invalid systems"
    (t/is (not (sut/usys-continuous? registry [:d :c :b :a])))
    (t/is (not (sut/usys-continuous? registry [:a :b2 :c])))
    (t/is (not (sut/usys-continuous? registry [:a :b3 :c3 :d])))

    (t/is (not (sut/usys-continuous? registry [:a])))))
