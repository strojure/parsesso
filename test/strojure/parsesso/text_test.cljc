(ns strojure.parsesso.text-test
  (:require [clojure.string :as string]
            [clojure.test :as test :refer [deftest testing]]
            [strojure.parsesso.core :as p]
            [strojure.parsesso.text :as t]))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

#_(test/run-tests)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

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
                              "unexpected \"d\""
                              "expecting character of \"abc\""]}

    (p (t/one-of "abc")
       "")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected end of input"
                              "expecting character of \"abc\""]}

    (p (t/one-of "abc" "a, b or c")
       "")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected end of input"
                              "expecting a, b or c"]}

    (p (t/one-of "a")
       "d")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected \"d\""
                              "expecting \"a\""]}

    (p (t/one-of "a")
       "")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected end of input"
                              "expecting \"a\""]}

    ))

(deftest none-of-t
  (test/are [expr result] (= result expr)

    (p (t/none-of "abc")
       "x")
    {:consumed true, :value (c "x")}

    (p (t/none-of "abc")
       "a")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected \"a\""
                              "expecting character of not \"abc\""]}

    (p (t/none-of "abc")
       "")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected end of input"
                              "expecting character of not \"abc\""]}

    (p (t/none-of "abc" "one of not a, b or c")
       "")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected end of input"
                              "expecting one of not a, b or c"]}

    (p (t/none-of "a")
       "a")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected \"a\""
                              "expecting not \"a\" character"]}

    (p (t/none-of "a")
       "")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected end of input"
                              "expecting not \"a\" character"]}

    ))

(deftest char-match-t
  (test/are [expr result] (= result expr)

    (p (p/many (t/char-match #"[a-z]"))
       "abc")
    {:consumed true, :value (seq "abc")}

    (p (t/char-match #"[a-z]")
       "A")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected \"A\""
                              "expecting character matching pattern #\"[a-z]\""]}

    (p (t/char-match #"[a-z]")
       "")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected end of input"
                              "expecting character matching pattern #\"[a-z]\""]}

    (p (t/char-match #"[a-z]" "a to z")
       "A")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected \"A\""
                              "expecting a to z"]}

    ))

(deftest string-t
  (test/are [expr result] (= result expr)

    (p (t/string "abc")
       "abc")
    {:consumed true, :value "abc"}

    (p (t/string "abc")
       "ab")
    {:consumed true, :error ["at line 1, column 1:"
                             "unexpected end of input"
                             "expecting \"abc\""]}

    (p (t/string "abc")
       "")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected end of input"
                              "expecting \"abc\""]}

    (p (t/string "abc")
       "abx")
    {:consumed true, :error ["at line 1, column 1:"
                             "unexpected \"x\""
                             "expecting \"abc\""]}

    (p (t/string "abc")
       "xyz")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected \"x\""
                              "expecting \"abc\""]}

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

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

(deftest whitespace-t
  (test/are [expr result] (= result expr)

    (p (p/many t/whitespace)
       " \t\r\n")
    {:consumed true, :value (seq " \t\r\n")}

    (p t/whitespace
       "a")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected \"a\""
                              "expecting whitespace character"]}

    (p t/whitespace
       "")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected end of input"
                              "expecting whitespace character"]}

    ))

(deftest skip-whites-t
  (test/are [expr result] (= result expr)

    (p t/skip-whites
       " \t\r\n")
    {:consumed true, :value nil}

    (p t/skip-whites
       "a")
    {:consumed false, :value nil}

    (p t/skip-whites
       "")
    {:consumed false, :value nil}

    (p t/some-skip-whites
       " \t\r\n")
    {:consumed true, :value nil}

    (p t/some-skip-whites
       "a")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected \"a\""
                              "expecting whitespace character"]}

    (p t/some-skip-whites
       "")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected end of input"
                              "expecting whitespace character"]}

    ))

(deftest newline-t
  (test/are [expr result] (= result expr)

    (p t/newline
       "\n")
    {:consumed true, :value (c "\n")}

    (p t/newline
       "\r\n")
    {:consumed true, :value (c "\n")}

    (p t/newline
       "\ra")
    {:consumed true, :error ["at line 1, column 2:"
                             "unexpected \"a\""
                             "expecting \"\\n\""]}

    (p t/newline
       "\r")
    {:consumed true, :error ["at line 1, column 2:"
                             "unexpected end of input"
                             "expecting \"\\n\""]}

    (p t/newline
       "a")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected \"a\""
                              "expecting \"\\n\" or \"\\r\""]}

    (p t/newline
       "")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected end of input"
                              "expecting \"\\n\" or \"\\r\""]}

    ))

(deftest alpha-t
  (test/are [expr result] (= result expr)

    (p t/alpha
       "a")
    {:consumed true, :value (c "a")}

    (p t/alpha
       "1")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected \"1\""
                              "expecting alphabetic character"]}

    (p t/alpha
       "")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected end of input"
                              "expecting alphabetic character"]}

    ))

(deftest upper-t
  (test/are [expr result] (= result expr)

    (p (p/many t/upper)
       "ABC")
    {:consumed true, :value (seq "ABC")}

    (p t/upper
       "a")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected \"a\""
                              "expecting upper case character"]}

    (p t/upper
       "")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected end of input"
                              "expecting upper case character"]}

    ))

(deftest lower-t
  (test/are [expr result] (= result expr)

    (p (p/many t/lower)
       "abc")
    {:consumed true, :value (seq "abc")}

    (p t/lower
       "A")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected \"A\""
                              "expecting lower case character"]}

    (p t/lower
       "")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected end of input"
                              "expecting lower case character"]}

    ))

(deftest numeric-t
  (test/are [expr result] (= result expr)

    (p (p/many t/numeric)
       "01234567890")
    {:consumed true, :value (seq "01234567890")}

    (p t/numeric
       "a")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected \"a\""
                              "expecting numeric character"]}

    (p t/numeric
       "")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected end of input"
                              "expecting numeric character"]}

    ))

(deftest alpha-numeric-t
  (test/are [expr result] (= result expr)

    (p (p/many t/alpha-numeric)
       "12345abcABC")
    {:consumed true, :value (seq "12345abcABC")}

    (p t/alpha-numeric
       "-")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected \"-\""
                              "expecting alphanumeric character"]}

    (p t/alpha-numeric
       "")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected end of input"
                              "expecting alphanumeric character"]}

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest plus-plus-t
  (test/are [expr result] (= result expr)

    (p (t/++ (t/one-of "abc"))
       "abc")
    {:consumed true, :value "a"}

    (p (t/++ (p/sequence [(p/some-many (t/one-of "abc"))
                          (p/some-many (t/one-of "123"))]))
       "abc123")
    {:consumed true, :value "abc123"}

    (p (t/++ (p/many (t/one-of "abc")))
       "123")
    {:consumed false, :value ""}

    (p (t/++ (t/one-of "abc"))
       "123")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected \"1\""
                              "expecting character of \"abc\""]}

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
