(ns perf.bench
  "Some benchmarks between parsesso, kern and parsatron. There is no jasentaa
  here because it is very slow."
  (:require [blancas.kern.core :as krn]
            [strojure.parsesso.char.core :as char]
            [strojure.parsesso.core :as p]
            [the.parsatron :as prt]))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn- prt-run
  [p input]
  (prt/run-parser p (prt/->InputState input (prt/->SourcePos 1 1))))

(def ^:private -input-10000 (repeat 10000 :a))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;;; Return value without parsing

(p/parse (p/result :x) [])
;             Execution time mean : 175,877194 ns
;    Execution time std-deviation : 34,075714 ns
;   Execution time lower quantile : 153,729903 ns ( 2,5%)
;   Execution time upper quantile : 217,875203 ns (97,5%)

(krn/parse (krn/return :x) [])
;             Execution time mean : 233,470315 ns
;    Execution time std-deviation : 66,244027 ns
;   Execution time lower quantile : 178,201399 ns ( 2,5%)
;   Execution time upper quantile : 326,518209 ns (97,5%)

(prt/run (prt/always :x) [])
;             Execution time mean : 168,392753 ns
;    Execution time std-deviation : 68,636364 ns
;   Execution time lower quantile : 123,449569 ns ( 2,5%)
;   Execution time upper quantile : 252,628602 ns (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;;; Fail immediately without parsing

(p/parse* (p/fail :x) [])
;             Execution time mean : 209,345385 ns
;    Execution time std-deviation : 73,822934 ns
;   Execution time lower quantile : 155,783672 ns ( 2,5%)
;   Execution time upper quantile : 298,985303 ns (97,5%)

(krn/parse (krn/fail :x) [])
;             Execution time mean : 386,590746 ns
;    Execution time std-deviation : 156,097460 ns
;   Execution time lower quantile : 266,519628 ns ( 2,5%)
;   Execution time upper quantile : 640,785168 ns (97,5%)

(prt-run (prt/never) [])
;             Execution time mean : 841,250545 ns
;    Execution time std-deviation : 206,671857 ns
;   Execution time lower quantile : 703,388694 ns ( 2,5%)
;   Execution time upper quantile : 1,115857 µs (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;;; Parse token

(p/parse (p/token #(= \a %)) "abc")
;             Execution time mean : 280,963465 ns
;    Execution time std-deviation : 16,328760 ns
;   Execution time lower quantile : 268,625666 ns ( 2,5%)
;   Execution time upper quantile : 307,169162 ns (97,5%)

(krn/parse (krn/satisfy #(= \a %)) "abc")
;             Execution time mean : 245,984170 ns
;    Execution time std-deviation : 13,553994 ns
;   Execution time lower quantile : 235,005603 ns ( 2,5%)
;   Execution time upper quantile : 268,329750 ns (97,5%)

(prt/run (prt/token #(= \a %)) "abc")
;             Execution time mean : 557,024259 ns
;    Execution time std-deviation : 14,359373 ns
;   Execution time lower quantile : 541,631508 ns ( 2,5%)
;   Execution time upper quantile : 578,875966 ns (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;;; Parse word

(p/parse (p/word "abc") "abc")
;             Execution time mean : 487,794555 ns
;    Execution time std-deviation : 125,625337 ns
;   Execution time lower quantile : 428,076023 ns ( 2,5%)
;   Execution time upper quantile : 705,007717 ns (97,5%)

(krn/parse (krn/token* "abc") "abc")
;             Execution time mean : 4,020720 µs
;    Execution time std-deviation : 429,048420 ns
;   Execution time lower quantile : 3,767589 µs ( 2,5%)
;   Execution time upper quantile : 4,754242 µs (97,5%)

(prt/run (prt/string "abc") "abc")
;             Execution time mean : 2,212562 µs
;    Execution time std-deviation : 91,094400 ns
;   Execution time lower quantile : 2,126279 µs ( 2,5%)
;   Execution time upper quantile : 2,342896 µs (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;;; Parse long word

(p/parse (p/word -input-10000) -input-10000)
;             Execution time mean : 218,488272 µs
;    Execution time std-deviation : 17,378108 µs
;   Execution time lower quantile : 201,064298 µs ( 2,5%)
;   Execution time upper quantile : 245,166269 µs (97,5%)

(comment
  (krn/parse (krn/token* -input-10000) -input-10000))
; Execution error (StackOverflowError) at blancas.kern.core/>>=$fn

(prt/run (prt/string -input-10000) -input-10000)
;             Execution time mean : 5,677465 ms
;    Execution time std-deviation : 961,844848 µs
;   Execution time lower quantile : 4,976587 ms ( 2,5%)
;   Execution time upper quantile : 6,805795 ms (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;;; Parse letters

(p/parse (p/many-zero char/alpha?) "abc")
;             Execution time mean : 991,422786 ns
;    Execution time std-deviation : 97,855368 ns
;   Execution time lower quantile : 894,100039 ns ( 2,5%)
;   Execution time upper quantile : 1,133213 µs (97,5%)

(krn/parse (krn/many krn/letter) "abc")
;             Execution time mean : 1,911586 µs
;    Execution time std-deviation : 511,124107 ns
;   Execution time lower quantile : 1,646502 µs ( 2,5%)
;   Execution time upper quantile : 2,783604 µs (97,5%)

(prt/run (prt/many (prt/letter)) "abc")
;             Execution time mean : 2,599675 µs
;    Execution time std-deviation : 576,904794 ns
;   Execution time lower quantile : 2,193151 µs ( 2,5%)
;   Execution time upper quantile : 3,354449 µs (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;;; Parse letters as string

(p/parse (p/with (p/many-zero char/alpha?) char/++) "abc")
;             Execution time mean : 1,647100 µs
;    Execution time std-deviation : 417,012141 ns
;   Execution time lower quantile : 1,375958 µs ( 2,5%)
;   Execution time upper quantile : 2,186531 µs (97,5%)

(krn/parse (krn/<+> (krn/many krn/letter)) "abc")
;             Execution time mean : 5,938304 µs
;    Execution time std-deviation : 251,680720 ns
;   Execution time lower quantile : 5,598434 µs ( 2,5%)
;   Execution time upper quantile : 6,200668 µs (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;;; Parse `many` for long input

(p/parse (p/many-zero (p/token #(= :a %))) -input-10000)
;             Execution time mean : 1,137599 ms
;    Execution time std-deviation : 67,330944 µs
;   Execution time lower quantile : 1,082947 ms ( 2,5%)
;   Execution time upper quantile : 1,221789 ms (97,5%)

(krn/parse (krn/many (krn/satisfy #(= :a %))) -input-10000)
;             Execution time mean : 2,011457 ms
;    Execution time std-deviation : 438,573991 µs
;   Execution time lower quantile : 1,681130 ms ( 2,5%)
;   Execution time upper quantile : 2,734318 ms (97,5%)

(prt/run (prt/many (prt/token #(= :a %))) -input-10000)
;             Execution time mean : 1,306567 sec
;    Execution time std-deviation : 131,786272 ms
;   Execution time lower quantile : 1,177467 sec ( 2,5%)
;   Execution time upper quantile : 1,476126 sec (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;;; Skip `many` for long input

(p/parse (p/skip-zero (p/token #(= :a %))) -input-10000)
;             Execution time mean : 1,043996 ms
;    Execution time std-deviation : 252,158552 µs
;   Execution time lower quantile : 893,890237 µs ( 2,5%)
;   Execution time upper quantile : 1,465919 ms (97,5%)

(krn/parse (krn/skip-many (krn/satisfy #(= :a %))) -input-10000)
;             Execution time mean : 1,416146 ms
;    Execution time std-deviation : 35,717820 µs
;   Execution time lower quantile : 1,379739 ms ( 2,5%)
;   Execution time upper quantile : 1,451345 ms (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;;; The `choice` combinator

(p/parse (p/choice (p/fail "a")
                   (p/fail "b")
                   (p/result :x)) [])
;             Execution time mean : 573,340067 ns
;    Execution time std-deviation : 46,346310 ns
;   Execution time lower quantile : 511,933832 ns ( 2,5%)
;   Execution time upper quantile : 624,550670 ns (97,5%)

(krn/parse (krn/<|> (krn/fail "a")
                    (krn/fail "a")
                    (krn/return :x)) [])
;             Execution time mean : 1,754808 µs
;    Execution time std-deviation : 148,221426 ns
;   Execution time lower quantile : 1,618505 µs ( 2,5%)
;   Execution time upper quantile : 1,924351 µs (97,5%)

(prt/run (prt/choice (prt/never)
                     (prt/never)
                     (prt/always :x)) [])
;             Execution time mean : 697,151006 ns
;    Execution time std-deviation : 165,879602 ns
;   Execution time lower quantile : 570,024598 ns ( 2,5%)
;   Execution time upper quantile : 961,147185 ns (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;;; Wrap with `expecting`

(p/parse (-> (p/result :x) (p/expecting "x")) [])
;             Execution time mean : 205,228035 ns
;    Execution time std-deviation : 25,200636 ns
;   Execution time lower quantile : 178,989629 ns ( 2,5%)
;   Execution time upper quantile : 241,378676 ns (97,5%)

(krn/parse (krn/<?> (krn/return :x) "x") [])
;             Execution time mean : 273,629608 ns
;    Execution time std-deviation : 30,460944 ns
;   Execution time lower quantile : 243,746902 ns ( 2,5%)
;   Execution time upper quantile : 320,003770 ns (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;;; Test for the end of input

(p/parse* p/eof " ")
;             Execution time mean : 231,661354 ns
;    Execution time std-deviation : 25,008376 ns
;   Execution time lower quantile : 209,952763 ns ( 2,5%)
;   Execution time upper quantile : 262,847436 ns (97,5%)

(krn/parse krn/eof " ")
;             Execution time mean : 1,428015 µs
;    Execution time std-deviation : 81,057937 ns
;   Execution time lower quantile : 1,352623 µs ( 2,5%)
;   Execution time upper quantile : 1,560179 µs (97,5%)

(prt-run (prt/eof) " ")
;             Execution time mean : 882,705676 ns
;    Execution time std-deviation : 46,738939 ns
;   Execution time lower quantile : 837,580307 ns ( 2,5%)
;   Execution time upper quantile : 948,317437 ns (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(p/parse (p/after (p/word "<!--")
                  (p/many-till p/any-token (p/maybe (p/word "-->"))))
         "<!-- comment -->")
;             Execution time mean : 7,944766 µs
;    Execution time std-deviation : 658,037391 ns
;   Execution time lower quantile : 7,133724 µs ( 2,5%)
;   Execution time upper quantile : 8,732943 µs (97,5%)

(krn/parse (krn/>> (krn/token* "<!--")
                   (krn/many-till krn/any-char (krn/<:> (krn/token* "-->"))))
           "<!-- comment -->")
;             Execution time mean : 84,653453 µs
;    Execution time std-deviation : 2,870985 µs
;   Execution time lower quantile : 81,222728 µs ( 2,5%)
;   Execution time upper quantile : 87,938498 µs (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
