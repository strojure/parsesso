(ns benchmarks.compare
  "Some benchmarks between parsesso, kern and parsatron. There is no jasentaa
  here because it is very slow."
  (:require [blancas.kern.core :as k]
            [strojure.parsesso.char :as char]
            [strojure.parsesso.parser :as p]
            [the.parsatron :as t]))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn- t-run
  [p input]
  (t/run-parser p (t/->InputState input (t/->SourcePos 1 1))))

(def ^:private -input-10000 (repeat 10000 :a))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;; ## Return value without parsing ##

(p/parse (p/result :x) [])
;             Execution time mean : 175,877194 ns
;    Execution time std-deviation : 34,075714 ns
;   Execution time lower quantile : 153,729903 ns ( 2,5%)
;   Execution time upper quantile : 217,875203 ns (97,5%)

(k/parse (k/return :x) [])
;             Execution time mean : 233,470315 ns
;    Execution time std-deviation : 66,244027 ns
;   Execution time lower quantile : 178,201399 ns ( 2,5%)
;   Execution time upper quantile : 326,518209 ns (97,5%)

(t/run (t/always :x) [])
;             Execution time mean : 168,392753 ns
;    Execution time std-deviation : 68,636364 ns
;   Execution time lower quantile : 123,449569 ns ( 2,5%)
;   Execution time upper quantile : 252,628602 ns (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;; ## Fail immediately without parsing ##

(p/parse* (p/fail :x) [])
;             Execution time mean : 188,952263 ns
;    Execution time std-deviation : 17,000877 ns
;   Execution time lower quantile : 172,453755 ns ( 2,5%)
;   Execution time upper quantile : 210,153699 ns (97,5%)

(k/parse (k/fail :x) [])
;             Execution time mean : 386,590746 ns
;    Execution time std-deviation : 156,097460 ns
;   Execution time lower quantile : 266,519628 ns ( 2,5%)
;   Execution time upper quantile : 640,785168 ns (97,5%)

(t-run (t/never) [])
;             Execution time mean : 841,250545 ns
;    Execution time std-deviation : 206,671857 ns
;   Execution time lower quantile : 703,388694 ns ( 2,5%)
;   Execution time upper quantile : 1,115857 µs (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;; ## Parse token ##

(p/parse (p/token #(= \a %)) "abc")
;             Execution time mean : 280,963465 ns
;    Execution time std-deviation : 16,328760 ns
;   Execution time lower quantile : 268,625666 ns ( 2,5%)
;   Execution time upper quantile : 307,169162 ns (97,5%)

(k/parse (k/satisfy #(= \a %)) "abc")
;             Execution time mean : 245,984170 ns
;    Execution time std-deviation : 13,553994 ns
;   Execution time lower quantile : 235,005603 ns ( 2,5%)
;   Execution time upper quantile : 268,329750 ns (97,5%)

(t/run (t/token #(= \a %)) "abc")
;             Execution time mean : 557,024259 ns
;    Execution time std-deviation : 14,359373 ns
;   Execution time lower quantile : 541,631508 ns ( 2,5%)
;   Execution time upper quantile : 578,875966 ns (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;; ## Parse word ##

(p/parse (p/word "abc") "abc")
;             Execution time mean : 492,578323 ns
;    Execution time std-deviation : 23,516467 ns
;   Execution time lower quantile : 471,832427 ns ( 2,5%)
;   Execution time upper quantile : 531,601202 ns (97,5%)

(k/parse (k/token* "abc") "abc")
;             Execution time mean : 4,020720 µs
;    Execution time std-deviation : 429,048420 ns
;   Execution time lower quantile : 3,767589 µs ( 2,5%)
;   Execution time upper quantile : 4,754242 µs (97,5%)

(t/run (t/string "abc") "abc")
;             Execution time mean : 2,212562 µs
;    Execution time std-deviation : 91,094400 ns
;   Execution time lower quantile : 2,126279 µs ( 2,5%)
;   Execution time upper quantile : 2,342896 µs (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;; ## Parse word, case-insensitive ##

(p/parse (p/word "abc" :ic) "ABC")
;             Execution time mean : 631,199580 ns
;    Execution time std-deviation : 9,939793 ns
;   Execution time lower quantile : 618,951019 ns ( 2,5%)
;   Execution time upper quantile : 641,954344 ns (97,5%)

(k/parse (k/token- "abc") "ABC")
;             Execution time mean : 5,063223 µs
;    Execution time std-deviation : 212,754488 ns
;   Execution time lower quantile : 4,915983 µs ( 2,5%)
;   Execution time upper quantile : 5,412170 µs (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;; ## Parse long word ##

(p/parse (p/word -input-10000) -input-10000)
;             Execution time mean : 190,951777 µs
;    Execution time std-deviation : 15,767078 µs
;   Execution time lower quantile : 177,441117 µs ( 2,5%)
;   Execution time upper quantile : 209,664373 µs (97,5%)

(comment
  (k/parse (k/token* -input-10000) -input-10000))
; Execution error (StackOverflowError) at blancas.kern.core/>>=$fn

(t/run (t/string -input-10000) -input-10000)
;             Execution time mean : 5,677465 ms
;    Execution time std-deviation : 961,844848 µs
;   Execution time lower quantile : 4,976587 ms ( 2,5%)
;   Execution time upper quantile : 6,805795 ms (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;; ## Parse letters ##

(p/parse (p/*many char/letter?) "abc")
;             Execution time mean : 975,326535 ns
;    Execution time std-deviation : 65,828611 ns
;   Execution time lower quantile : 915,594047 ns ( 2,5%)
;   Execution time upper quantile : 1,059000 µs (97,5%)

(k/parse (k/many k/letter) "abc")
;             Execution time mean : 1,911586 µs
;    Execution time std-deviation : 511,124107 ns
;   Execution time lower quantile : 1,646502 µs ( 2,5%)
;   Execution time upper quantile : 2,783604 µs (97,5%)

(t/run (t/many (t/letter)) "abc")
;             Execution time mean : 2,599675 µs
;    Execution time std-deviation : 576,904794 ns
;   Execution time lower quantile : 2,193151 µs ( 2,5%)
;   Execution time upper quantile : 3,354449 µs (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;; ## Parse letters as string ##

(p/parse (-> (p/*many char/letter?) (p/value char/str*)) "abc")
;             Execution time mean : 1,514160 µs
;    Execution time std-deviation : 104,898493 ns
;   Execution time lower quantile : 1,439323 µs ( 2,5%)
;   Execution time upper quantile : 1,680704 µs (97,5%)

(k/parse (k/<+> (k/many k/letter)) "abc")
;             Execution time mean : 5,568215 µs
;    Execution time std-deviation : 145,037838 ns
;   Execution time lower quantile : 5,459951 µs ( 2,5%)
;   Execution time upper quantile : 5,810555 µs (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;; ## Parse `many` for long input ##

(p/parse (p/*many (p/token #(= :a %))) -input-10000)
;             Execution time mean : 1,311809 ms
;    Execution time std-deviation : 96,377398 µs
;   Execution time lower quantile : 1,223376 ms ( 2,5%)
;   Execution time upper quantile : 1,426319 ms (97,5%)

(k/parse (k/many (k/satisfy #(= :a %))) -input-10000)
;             Execution time mean : 1,928105 ms
;    Execution time std-deviation : 62,373984 µs
;   Execution time lower quantile : 1,868339 ms ( 2,5%)
;   Execution time upper quantile : 2,024112 ms (97,5%)

(t/run (t/many (t/token #(= :a %))) -input-10000)
;             Execution time mean : 1,066323 sec
;    Execution time std-deviation : 159,363140 ms
;   Execution time lower quantile : 984,876092 ms ( 2,5%)
;   Execution time upper quantile : 1,341844 sec (97,5%)
;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;; ## Skip `many` for long input ##

(p/parse (p/*skip (p/token #(= :a %))) -input-10000)
;             Execution time mean : 1,043996 ms
;    Execution time std-deviation : 252,158552 µs
;   Execution time lower quantile : 893,890237 µs ( 2,5%)
;   Execution time upper quantile : 1,465919 ms (97,5%)

(k/parse (k/skip-many (k/satisfy #(= :a %))) -input-10000)
;             Execution time mean : 1,416146 ms
;    Execution time std-deviation : 35,717820 µs
;   Execution time lower quantile : 1,379739 ms ( 2,5%)
;   Execution time upper quantile : 1,451345 ms (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;; ## The `alt` combinator ##

(p/parse (p/alt (p/fail "a")
                (p/fail "b")
                (p/result :x)) [])
;             Execution time mean : 573,340067 ns
;    Execution time std-deviation : 46,346310 ns
;   Execution time lower quantile : 511,933832 ns ( 2,5%)
;   Execution time upper quantile : 624,550670 ns (97,5%)

(k/parse (k/<|> (k/fail "a")
                (k/fail "b")
                (k/return :x)) [])
;             Execution time mean : 1,754808 µs
;    Execution time std-deviation : 148,221426 ns
;   Execution time lower quantile : 1,618505 µs ( 2,5%)
;   Execution time upper quantile : 1,924351 µs (97,5%)

(t/run (t/choice (t/never)
                 (t/never)
                 (t/always :x)) [])
;             Execution time mean : 697,151006 ns
;    Execution time std-deviation : 165,879602 ns
;   Execution time lower quantile : 570,024598 ns ( 2,5%)
;   Execution time upper quantile : 961,147185 ns (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;; ## Wrap with `expecting` ##

(p/parse (-> (p/result :x) (p/expecting "x")) [])
;             Execution time mean : 212,033445 ns
;    Execution time std-deviation : 20,071125 ns
;   Execution time lower quantile : 196,685023 ns ( 2,5%)
;   Execution time upper quantile : 238,117212 ns (97,5%)

(k/parse (k/<?> (k/return :x) "x") [])
;             Execution time mean : 222,587325 ns
;    Execution time std-deviation : 16,924812 ns
;   Execution time lower quantile : 205,615791 ns ( 2,5%)
;   Execution time upper quantile : 240,220579 ns (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;; ## Test for the end of input ##

(p/parse* p/eof " ")
;             Execution time mean : 231,661354 ns
;    Execution time std-deviation : 25,008376 ns
;   Execution time lower quantile : 209,952763 ns ( 2,5%)
;   Execution time upper quantile : 262,847436 ns (97,5%)

(k/parse k/eof " ")
;             Execution time mean : 1,428015 µs
;    Execution time std-deviation : 81,057937 ns
;   Execution time lower quantile : 1,352623 µs ( 2,5%)
;   Execution time upper quantile : 1,560179 µs (97,5%)

(t-run (t/eof) " ")
;             Execution time mean : 882,705676 ns
;    Execution time std-deviation : 46,738939 ns
;   Execution time lower quantile : 837,580307 ns ( 2,5%)
;   Execution time upper quantile : 948,317437 ns (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(p/parse (p/after (p/word "<!--")
                  (p/*many-till p/any-token (p/maybe (p/word "-->"))))
         "<!-- comment -->")
;             Execution time mean : 7,450434 µs
;    Execution time std-deviation : 607,080144 ns
;   Execution time lower quantile : 6,900613 µs ( 2,5%)
;   Execution time upper quantile : 8,221736 µs (97,5%)

(k/parse (k/>> (k/token* "<!--")
               (k/many-till k/any-char (k/<:> (k/token* "-->"))))
         "<!-- comment -->")
;             Execution time mean : 84,653453 µs
;    Execution time std-deviation : 2,870985 µs
;   Execution time lower quantile : 81,222728 µs ( 2,5%)
;   Execution time upper quantile : 87,938498 µs (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
