# chrono

Pure clojure time made simple for clj & cljs

<img src="clocks.png" height="384px" />

## Usage

```clj
(require '[chrono.core :as ch]
         '[chrono.datetime :as cd]
         '[chrono.interval :as ci])

(def t
  #::cd{:year  2018
        :month 1
        :day   29
        :hour  10
        :min   30
        :sec   15
        :tz    0})

(ch/+ t {::ci/min 100})  ;; => #::cd{:year 2018, :month 1, :day 29, :hour 12, :min 10, :sec 15, :tz 0}
(ch/+ t {::ci/min -100}) ;; => #::cd{:year 2018, :month 1, :day 29, :hour 8, :min 50, :sec 15, :tz 0}
;; also there are = not= > >= < <= -

(ch/normalize {::cd/min 100}) ;; => #::cd{:min 40, :hour 1}

(def iso
  [::cd/year \- ::cd/month \- ::cd/day \T
   ::cd/hour \: ::cd/min \: ::cd/sec])
(ch/format t iso) ;; => "2018-01-29T10:30:15"
(ch/parse "2018-01-29T10:30:15" iso) ;; => #::cd{:year 2018, :month 1, :day 29, :hour 10, :min 30, :sec 15}

(ch/format t [::cd/day "/" ::cd/month "/" ::cd/year]) ;; => "29/01/2018"
(ch/parse "2018.01.29" [::cd/year "." ::cd/month "." ::cd/day]) ;; => {:year 2018, :month 1, :day 29}

;You can specify leading zero padding width by passing pairs [keyword number]
(ch/format #::cd{:hour 1 :min 0 :sec 5}
           [[::cd/hour 1] \: [::cd/min 1] \: [::cd/sec 1]]) ;; => "1:0:5"

(-> t
    (ch/to-tz :ny) ;; => #::cd{:year 2018, :month 1, :day 29, :hour 5, :min 30, :sec 15, :tz :ny}
    (ch/to-utc))   ;; => #::cd{:year 2018, :month 1, :day 29, :hour 10, :min 30, :sec 15, :tz :0}
;; implement your tz with defmethod ch/day-saving :<your-tz>

;; You can use number as utc-offset
(-> t
    (ch/to-tz 3) ;; => #::cd{:year 2018, :month 1, :day 29, :hour 13, :min 30, :sec 15, :tz 3}
    (ch/to-utc)) ;; => #::cd{:year 2018, :month 1, :day 29, :hour 10, :min 30, :sec 15, :tz 0}

(require '[chrono.now :as now])

(now/local)     ;; => #::cd{:year 2019, :month 9, :day 18, :hour 1, :min 44, :sec 34, :ms 768, :tz 2}
(now/utc)       ;; => #::cd{:year 2019, :month 9, :day 17, :hour 23, :min 44, :sec 34, :ms 768, :tz 0}
(now/today)     ;; => #::cd{:year 2019, :month 9, :day 18, :tz 2}
(now/utc-today) ;; => #::cd{:year 2019, :month 9, :day 17, :tz 0}

(now/tz-offset) ;; => #::ci{:hour 2}

(ch/= (assoc (ch/+ (now/utc) (now/tz-offset))
             ::cd/tz (::ci/hour (now/tz-offset)))
      (now/local)) ;; => true

;; using custom units
;; Add custom normalization method. Example for nanoseconds:
(require '[chrono.ops :as ops])
(def normalize-ns (ops/gen-norm ::cd/ns ::cd/ms 1000000 0))
(defmethod ops/normalize-rule ::cd/ns [_ t] (normalize-ns t))

(ops/normalize {::cd/ns 1000000000}          ;; => #::cd{:sec 1}
(ch/+ {::cd/ns 999999999} {::ci/ns 1})       ;; => #::cd{:sec 1}
(ch/+ {::cd/ns 9999999} {::ci/ns 999000001}) ;; => #::cd{:sec 1 :ms 9}
```

## License

Copyright © 2018 niquola

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
