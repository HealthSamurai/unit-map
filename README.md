# chrono

Pure clojure time made simple for clj & cljs

![clock](clocks.png)

## Usage

```clj
(require '[chrono.core :as ch])

(def t
  {:year  2018
   :month 1
   :day   29
   :hour  10
   :min   30
   :sec   15
   :tz    :utc})

(ch/+ t {:min 100}) ;; => {:min 40, :day 29, :hour 11, :second 15, :month 1, :year 2018, :minutes 30}
(ch/+ t {:min -100}) ;; => {:min 20, :day 29, :hour 8, :second 15, :month 1, :year 2018, :minutes 30}
;; also there is = not= > >= < <=

(ch/normalize {:min 100}) ;; => {:min 40, :hour 1}

(def iso [:year \- :month \- :day \T :hour \: :min \: :sec])
(ch/format t iso) ;; => "2018-01-29T10:30:15"
(ch/parse "2018-01-29T10:30:15" iso) ;; => {:year 2018, :month 1, :day 29, :hour 10, :min 30, :sec 15}

(ch/format t [:day "/" :month "/" :year]) ;; => "29/01/2018"
(ch/parse  "2018.01.29"  [:year "." :month "." :day]) ;; => {:year 2018, :month 1, :day 29}

(require '[chrono.tz :as tz])

(-> t
    (tz/to-tz :ny) ;; => {:min 30, :day 29, :hour 5, :month 1, :year 2018, :sec 15, :tz :ny}
    (tz/to-utc))   ;; => {:min 30, :day 29, :hour 10, :month 1, :year 2018, :sec 15}
;; implement your tz with defmethod tz/day-saving :<your-tz>

(require '[chrono.now :as now])

(now/local)     ;; => {:year 2019, :month 9, :day 18, :hour 1, :min 44, :sec 34, :ms 768}
(now/utc)       ;; => {:year 2019, :month 9, :day 17, :hour 23, :min 44, :sec 34, :ms 768}
(now/today)     ;; => {:year 2019, :month 9, :day 18}
(now/utc-today) ;; => {:year 2019, :month 9, :day 17}

(now/tz-offset) ;; => {:hour 2}

(ch/= (ch/+ (now/utc) (now/tz-offset))
      (now/local)) ;; => true


```

## License

Copyright © 2018 niquola

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
