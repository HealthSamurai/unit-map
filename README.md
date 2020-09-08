# chrono

Pure clojure time made simple for clj & cljs

<img src="clocks.png" height="384px" />

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
   :ms    133})

(ch/+ t ^:delta{:min 100}) ;; => {:year 2018, :month 1, :day 29, :hour 12, :min 10, :sec 15, :ms 133}
(ch/+ t ^:delta{:min -100}) ;; => {:year 2018, :month 1, :day 29, :hour 8, :min 50, :sec 15, :ms 133}
;; also there is = not= > >= < <= -

(ch/normalize {:min 100}) ;; => {:min 40, :hour 1}

(def iso [:year \- :month \- :day \T :hour \: :min \: :sec \. :ms])
(ch/format t iso) ;; => "2018-01-29T10:30:15.133"
(ch/parse "2018-01-29T10:30:15.133" iso) ;; => {:year 2018, :month 1, :day 29, :hour 10, :min 30, :sec 15, :ms 133}

(ch/format t [:day "/" :month "/" :year]) ;; => "29/01/2018"
(ch/parse  "2018.01.29"  [:year "." :month "." :day]) ;; => {:year 2018, :month 1, :day 29}

;; You can specify some format/parse options by passing a vector:
;; - padding width with an integer
;; - padding string with a string or a char (default is 0 for numbers and \space for strings)
;; - custom formatting function as a function
;; - locale with meta
(ch/format {:hour 1 :min 0 :sec 5} [[:hour 1] \: [:min 1] \: [:sec 1]]) ;; => "1:0:5"
(ch/format t
           [[:day 0] " " ^:en[:month] " " [:year 2]
            "\n"
            [:hour 30 "_" (fn [value fmt-el] (str "your formatting logic"))]])
;; => "29 January 18\n_________your formatting logic"

(require '[chrono.now :as now])

(now/local)     ;; => {:year 2020, :month 9, :day 8, :hour 20, :min 3, :sec 27, :ms 846, :tz {:hour 2}}
(now/utc)       ;; => {:year 2020, :month 9, :day 8, :hour 18, :min 3, :sec 27, :ms 846, :tz {:hour 0}}
(now/today)     ;; => {:year 2020, :month 9, :day 8, :tz {:hour 2}}
(now/utc-today) ;; => {:year 2020, :month 9, :day 8, :tz {:hour 0}}
(now/now)       ;; => {:hour 20, :min 3, :sec 27, :ms 846, :tz {:hour 2}}
(now/utc-now)   ;; => {:hour 18, :min 3, :sec 27, :ms 846, :tz {:hour 0}}
(now/tz-offset) ;; => {:hour 2}


readme TODO:
 - locale
 - ops
 - custom calendars
```

## License

Copyright © 2018 niquola

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
