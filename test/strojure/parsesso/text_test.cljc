(ns strojure.parsesso.text-test
  (:require [clojure.string :as string]
            [clojure.test :as test :refer [deftest testing]]
            [strojure.parsesso.core :as p]
            [strojure.parsesso.text :as t]))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(comment
  (test/run-tests))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;; TODO: Look at pos and remaining input.
(defn- p
  "Parses test input using given parser. Returns custom map with test result."
  [parser input]
  (let [result (t/parse parser input)]
    (if (p/error? result)
      (-> (select-keys result [:consumed])
          (assoc :error (-> (:error result) (str) (string/split-lines))))
      (select-keys result [:consumed :value]))))

(defn- c
  "Cross-platform char."
  [s]
  #?(:cljs s, :clj (first s)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest char-t
  (test/are [expr result] (= result expr)

    (p (t/char \a)
       "a")
    {:consumed true, :value (c "a")}

    (p (t/char \a)
       "b")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected \"b\""
                              "expecting \"a\""]}

    (p (t/char \a)
       "")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected end of input"
                              "expecting \"a\""]}

    ))

(deftest any-char-t
  (test/are [expr result] (= result expr)

    (p t/any-char
       "a")
    {:consumed true, :value (c "a")}

    (p t/any-char
       "")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected end of input"]}

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest one-of-t
  (test/are [expr result] (= result expr)

    (p (t/one-of "abc")
       "a")
    {:consumed true, :value (c "a")}

    (p (t/one-of "abc")
       "b")
    {:consumed true, :value (c "b")}

    (p (t/one-of "abc")
       "c")
    {:consumed true, :value (c "c")}

    (p (t/one-of "abc")
       "d")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected \"d\""]}

    (p (t/one-of "abc")
       "")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected end of input"]}

    ))

(deftest none-of-t
  (test/are [expr result] (= result expr)

    (p (t/none-of "abc")
       "x")
    {:consumed true, :value (c "x")}

    (p (t/none-of "abc")
       "a")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected \"a\""]}

    (p (t/none-of "abc")
       "")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected end of input"]}

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
