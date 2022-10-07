(ns strojure.parsesso.char.core-test
  (:require [clojure.string :as string]
            [clojure.test :as test :refer [deftest]]
            [strojure.parsesso.char.core :as char]
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

    (p (p/token (char/one-of? "abc"))
       "a")
    {:consumed true, :value (c "a")}

    (p (p/token (char/one-of? "abc"))
       "b")
    {:consumed true, :value (c "b")}

    (p (p/token (char/one-of? "abc"))
       "c")
    {:consumed true, :value (c "c")}

    (p (p/token (char/one-of? "abc"))
       "d")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected \"d\""
                              "expecting character of \"abc\""]}

    (p (p/token (char/one-of? "abc"))
       "")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected end of input"
                              "expecting character of \"abc\""]}

    (p (p/token (char/one-of? "a"))
       "d")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected \"d\""
                              "expecting \"a\""]}

    (p (p/token (char/one-of? "a"))
       "")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected end of input"
                              "expecting \"a\""]}

    ))

(deftest not-of?-t
  (test/are [expr result] (= result expr)

    (p (p/token (char/not-of? "abc"))
       "x")
    {:consumed true, :value (c "x")}

    (p (p/token (char/not-of? "abc"))
       "a")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected \"a\""
                              "expecting character not of \"abc\""]}

    (p (p/token (char/not-of? "abc"))
       "")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected end of input"
                              "expecting character not of \"abc\""]}

    (p (p/token (char/not-of? "a"))
       "a")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected \"a\""
                              "expecting not \"a\" character"]}

    (p (p/token (char/not-of? "a"))
       "")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected end of input"
                              "expecting not \"a\" character"]}

    ))

(deftest re-match?-t
  (test/are [expr result] (= result expr)

    (p (p/many-zero (p/token (char/re-match? #"[a-z]")))
       "abc")
    {:consumed true, :value (seq "abc")}

    (p (p/token (char/re-match? #"[a-z]"))
       "A")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected \"A\""
                              "expecting character matching pattern #\"[a-z]\""]}

    (p (p/token (char/re-match? #"[a-z]"))
       "")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected end of input"
                              "expecting character matching pattern #\"[a-z]\""]}

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest alpha?-t
  (test/are [expr result] (= result expr)

    (p (p/token char/alpha?)
       "a")
    {:consumed true, :value (c "a")}

    (p (p/token char/alpha?)
       "1")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected \"1\""
                              "expecting alphabetic character"]}

    (p (p/token char/alpha?)
       "")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected end of input"
                              "expecting alphabetic character"]}

    ))

(deftest upper?-t
  (test/are [expr result] (= result expr)

    (p (p/many-zero (p/token char/upper?))
       "ABC")
    {:consumed true, :value (seq "ABC")}

    (p (p/token char/upper?)
       "a")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected \"a\""
                              "expecting upper case character"]}

    (p (p/token char/upper?)
       "")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected end of input"
                              "expecting upper case character"]}

    ))

(deftest lower?-t
  (test/are [expr result] (= result expr)

    (p (p/many-zero (p/token char/lower?))
       "abc")
    {:consumed true, :value (seq "abc")}

    (p (p/token char/lower?)
       "A")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected \"A\""
                              "expecting lower case character"]}

    (p (p/token char/lower?)
       "")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected end of input"
                              "expecting lower case character"]}

    ))

(deftest numeric?-t
  (test/are [expr result] (= result expr)

    (p (p/many-zero (p/token char/numeric?))
       "01234567890")
    {:consumed true, :value (seq "01234567890")}

    (p (p/token char/numeric?)
       "a")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected \"a\""
                              "expecting numeric character"]}

    (p (p/token char/numeric?)
       "")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected end of input"
                              "expecting numeric character"]}

    ))

(deftest alpha-numeric?-t
  (test/are [expr result] (= result expr)

    (p (p/many-zero (p/token char/alpha-numeric?))
       "12345abcABC")
    {:consumed true, :value (seq "12345abcABC")}

    (p (p/token char/alpha-numeric?)
       "-")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected \"-\""
                              "expecting alphanumeric character"]}

    (p (p/token char/alpha-numeric?)
       "")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected end of input"
                              "expecting alphanumeric character"]}

    ))

(deftest whitespace?-t
  (test/are [expr result] (= result expr)

    (p (p/many-zero (p/token char/whitespace?))
       " \t\r\n")
    {:consumed true, :value (seq " \t\r\n")}

    (p (p/token char/whitespace?)
       "a")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected \"a\""
                              "expecting whitespace character"]}

    (p (p/token char/whitespace?)
       "")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected end of input"
                              "expecting whitespace character"]}

    ))

(deftest newline-t
  (test/are [expr result] (= result expr)

    (p char/newline
       "\n")
    {:consumed true, :value (c "\n")}

    (p char/newline
       "\r\n")
    {:consumed true, :value (c "\n")}

    (p char/newline
       "\ra")
    {:consumed true, :error ["error at line 1, column 2:"
                             "unexpected \"a\""
                             "expecting \"\\n\""]}

    (p char/newline
       "\r")
    {:consumed true, :error ["error at line 1, column 2:"
                             "unexpected end of input"
                             "expecting \"\\n\""]}

    (p char/newline
       "a")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected \"a\""
                              "expecting \"\\n\" or \"\\r\""]}

    (p char/newline
       "")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected end of input"
                              "expecting \"\\n\" or \"\\r\""]}

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest ++-t
  (test/are [expr result] (= result expr)

    (p (char/++ (p/token (char/one-of? "abc")))
       "abc")
    {:consumed true, :value "a"}

    (p (char/++ (p/each [(p/many-some (p/token (char/one-of? "abc")))
                         (p/many-some (p/token (char/one-of? "123")))]))
       "abc123")
    {:consumed true, :value "abc123"}

    (p (char/++ (p/many-zero (p/token (char/one-of? "abc"))))
       "123")
    {:consumed false, :value ""}

    (p (char/++ (p/token (char/one-of? "abc")))
       "123")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected \"1\""
                              "expecting character of \"abc\""]}

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,