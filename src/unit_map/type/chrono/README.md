# chrono

Pure clojure time made simple for clj & cljs

<img src="clocks.png" height="384px"/>

## Usage

```clj
(require '[unit-map.type.chrono.datetime :as datetime]
         '[unit-map.ops :as ops])

(def t ^::datetime/military{:year 2018, :month 1, :day 29, :hour 10, :min 30, :sec 15, :ms 133})

(ops/plus t ^{::datetime/military :delta}{:min 100}) ;; => {:year 2018, :month 1, :day 29, :hour 12, :min 10, :sec 15, :ms 133}
;; also there are minus, eq?, lt?, gt?, lte?, gte?, cmp


;; You can define default type for whole project
(defmethod ops/definition :unit-map.type/default [_] datetime/gregorian-military)

(def t' {:year 2018, :month 1, :day 29, :hour 10, :min 30})

(ops/plus t' ^:delta{:min 100}) ;; => {:year 2018, :month 1, :day 29, :hour 12, :min 10, :sec 15, :ms 133}


;; If you've got bad value, you can fix it with normalizae:
(ops/normalize {:min 1337}) ;; => {:min 17, :hour 22}


(require '[unit-map.io :as io])

(def iso [:year \- :month \- :day \T :hour \: :min \: :sec \. :ms])

(io/format t iso)                                     ;; => "2018-01-29T10:30:15.133"
(io/parse "2018-01-29T10:30:15.133" iso)              ;; => {:year 2018, :month 1, :day 29, :hour 10, :min 30, :sec 15, :ms 133}
(io/format t [:day "/" :month "/" :year])             ;; => "29/01/2018"
(io/parse  "2018.01.29"  [:year "." :month "." :day]) ;; => {:year 2018, :month 1, :day 29}

;; You can specify some format/parse options by passing a vector:
;; - padding width with an integer
;; - padding string with a string or a char (default is 0 for numbers and \space for strings)
;; - custom formatting function as a function
;; - locale with meta
(io/format {:hour 1 :min 0 :sec 5} [[:hour 1] \: [:min 1] \: [:sec 1]]) ;; => "1:0:5"
(io/format t
           [[:day 0] " " ^:en[:month] " " [:year 2]
            "\n"
            [:hour 30 "_" (fn [value fmt-el] (str "your formatting logic"))]])
;; => "29 January 18\n_________your formatting logic"


(require '[unit-map.type.chrono.util.now :as now])

(now/local)     ;; => {:year 2020, :month 9, :day 8, :hour 20, :min 3, :sec 27, :ms 846, :tz {:hour 2}}
(now/utc)       ;; => {:year 2020, :month 9, :day 8, :hour 18, :min 3, :sec 27, :ms 846, :tz {:hour 0}}
(now/today)     ;; => {:year 2020, :month 9, :day 8, :tz {:hour 2}}
(now/utc-today) ;; => {:year 2020, :month 9, :day 8, :tz {:hour 0}}
(now/now)       ;; => {:hour 20, :min 3, :sec 27, :ms 846, :tz {:hour 2}}
(now/utc-now)   ;; => {:hour 18, :min 3, :sec 27, :ms 846, :tz {:hour 0}}
(now/tz-offset) ;; => {:hour 2}
```

TODO:
 - locale readme
 - custom calendars readme

## License

Copyright © 2018 niquola

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
