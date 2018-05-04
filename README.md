# chrono

Pure clojure time implementation for clj & cljs

## Usage

```clj


(def t (ch/time
          {:year 2018 
           :month 1
           :day 29 
           :hours 10 
           :minutes 30 
           :second 15
           :tz :utc}))

(ch/now) => time
        
(ch/add t {:minutes 100}) => time        
(ch/add t {:minutes -100}) => time        

(ch/to-tz t :ny)
(ch/to-tz t :utc)

(def iso [:year "-" :month "-" :day "T" :h ":" :min ":" :sec])
(ch/format t iso) 
(ch/parse t iso) 

(ch/format t [:day "/" :month "/" :year])
(ch/parse  t  [:year "." :month "." :day])



```

## License

Copyright © 2018 niquola

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
