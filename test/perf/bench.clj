(ns perf.bench
  (:require [blancas.kern.core :as krn]
            [strojure.parsesso.char.core :as char]
            [strojure.parsesso.core :as p]
            [the.parsatron :as ptr]))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn- ptr-run
  [p input]
  (ptr/run-parser p (ptr/->InputState input (ptr/->SourcePos 1 1))))

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

(ptr/run (ptr/always :x) [])
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

(ptr-run (ptr/never) [])
;             Execution time mean : 841,250545 ns
;    Execution time std-deviation : 206,671857 ns
;   Execution time lower quantile : 703,388694 ns ( 2,5%)
;   Execution time upper quantile : 1,115857 µs (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;;; Parse exact string

(p/parse (p/word "abc") "abc")
;             Execution time mean : 487,794555 ns
;    Execution time std-deviation : 125,625337 ns
;   Execution time lower quantile : 428,076023 ns ( 2,5%)
;   Execution time upper quantile : 705,007717 ns (97,5%)

(krn/parse (krn/token- "abc") "abc")
;             Execution time mean : 4,648685 µs
;    Execution time std-deviation : 253,817422 ns
;   Execution time lower quantile : 4,401602 µs ( 2,5%)
;   Execution time upper quantile : 5,037056 µs (97,5%)

(ptr/run (ptr/string "abc") "abc")
;             Execution time mean : 2,212562 µs
;    Execution time std-deviation : 91,094400 ns
;   Execution time lower quantile : 2,126279 µs ( 2,5%)
;   Execution time upper quantile : 2,342896 µs (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;;; Parse letters

(p/parse (p/many-zero (p/token char/alpha?)) "abc")
;             Execution time mean : 906,599211 ns
;    Execution time std-deviation : 185,143135 ns
;   Execution time lower quantile : 690,083851 ns ( 2,5%)
;   Execution time upper quantile : 1,156656 µs (97,5%)

(krn/parse (krn/many krn/letter) "abc")
;             Execution time mean : 1,911586 µs
;    Execution time std-deviation : 511,124107 ns
;   Execution time lower quantile : 1,646502 µs ( 2,5%)
;   Execution time upper quantile : 2,783604 µs (97,5%)

(ptr/run (ptr/many (ptr/letter)) "abc")
;             Execution time mean : 3,017004 µs
;    Execution time std-deviation : 786,985030 ns
;   Execution time lower quantile : 2,512974 µs ( 2,5%)
;   Execution time upper quantile : 4,307684 µs (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;;; Parse letters as string

(p/parse (char/++ (p/many-zero (p/token char/alpha?))) "abc")
;             Execution time mean : 1,466334 µs
;    Execution time std-deviation : 338,852792 ns
;   Execution time lower quantile : 1,217230 µs ( 2,5%)
;   Execution time upper quantile : 1,943848 µs (97,5%)

(krn/parse (krn/<+> (krn/many krn/letter)) "abc")
;             Execution time mean : 5,938304 µs
;    Execution time std-deviation : 251,680720 ns
;   Execution time lower quantile : 5,598434 µs ( 2,5%)
;   Execution time upper quantile : 6,200668 µs (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;;; Parse `many` for long input

(def ^:private -input-10000 (repeat 10000 :a))

(p/parse (p/many-zero (p/token #(= :a %))) -input-10000)
;             Execution time mean : 1,397084 ms
;    Execution time std-deviation : 413,081039 µs
;   Execution time lower quantile : 1,197764 ms ( 2,5%)
;   Execution time upper quantile : 2,113904 ms (97,5%)

(krn/parse (krn/many (krn/satisfy #(= :a %))) -input-10000)
;             Execution time mean : 2,011457 ms
;    Execution time std-deviation : 438,573991 µs
;   Execution time lower quantile : 1,681130 ms ( 2,5%)
;   Execution time upper quantile : 2,734318 ms (97,5%)

(ptr/run (ptr/many (ptr/token #(= :a %))) -input-10000)
;             Execution time mean : 1,306567 sec
;    Execution time std-deviation : 131,786272 ms
;   Execution time lower quantile : 1,177467 sec ( 2,5%)
;   Execution time upper quantile : 1,476126 sec (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;;; Skip `many` for long input

(p/parse (p/skip-zero (p/token #(= :a %))) -input-10000)
;             Execution time mean : 1,124563 ms
;    Execution time std-deviation : 311,045788 µs
;   Execution time lower quantile : 956,230181 µs ( 2,5%)
;   Execution time upper quantile : 1,659816 ms (97,5%)

(krn/parse (krn/skip-many (krn/satisfy #(= :a %))) -input-10000)
;             Execution time mean : 1,416146 ms
;    Execution time std-deviation : 35,717820 µs
;   Execution time lower quantile : 1,379739 ms ( 2,5%)
;   Execution time upper quantile : 1,451345 ms (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
