(ns strojure.parsesso.char.insensitive-test
  (:require [clojure.string :as string]
            [clojure.test :as test :refer [deftest]]
            [strojure.parsesso.char.insensitive :as char*]
            [strojure.parsesso.core :as p]))

#_(test/run-tests)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn- p
  "Parses test input using given parser. Returns custom map with test result."
  [parser input]
  (let [result (p/parse* parser input)]
    (if-let [error (:error result)]
      (-> (select-keys result [:consumed])
          (assoc :error (-> (str error) (string/split-lines))))
      (select-keys result [:consumed :value]))))

(defn- c
  "Cross-platform char."
  [s]
  #?(:cljs s, :clj (first s)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest one-of?-t
  (test/are [expr result] (= result expr)

    (p (char*/one-of? "abc")
       "a")
    {:consumed true, :value (c "a")}

    (p (char*/one-of? "abc")
       "A")
    {:consumed true, :value (c "A")}

    (p (char*/one-of? "ABC")
       "a")
    {:consumed true, :value (c "a")}

    (p (char*/one-of? "abc")
       "d")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected \"d\""
                              "expecting character of \"abc\""]}

    ))

(deftest not-of?-t
  (test/are [expr result] (= result expr)

    (p (char*/not-of? "abc")
       "x")
    {:consumed true, :value (c "x")}

    (p (char*/not-of? "abc")
       "a")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected \"a\""
                              "expecting character not of \"abc\""]}

    (p (char*/not-of? "abc")
       "A")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected \"A\""
                              "expecting character not of \"abc\""]}

    (p (char*/not-of? "a")
       "a")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected \"a\""
                              "expecting not \"a\" character"]}

    (p (char*/not-of? "a")
       "A")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected \"A\""
                              "expecting not \"a\" character"]}

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest word-t
  (test/are [expr result] (= result expr)

    (p (char*/word "abc")
       "abc")
    {:consumed true, :value "abc"}

    (p (char*/word "abc")
       "ABC")
    {:consumed true, :value "abc"}

    (p (char*/word "ABC")
       "abc")
    {:consumed true, :value "ABC"}

    (p (char*/word "abc")
       "abd")
    {:consumed true, :error ["error at line 1, column 1:"
                             "unexpected \"d\""
                             "expecting \"abc\""]}

    (p (char*/word "abc")
       "ab")
    {:consumed true, :error ["error at line 1, column 1:"
                             "unexpected end of input"
                             "expecting \"abc\""]}

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
