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

(deftest char-of-t
  (test/are [expr result] (= result expr)

    (p (t/char-of "abc")
       "a")
    {:consumed true, :value (c "a")}

    (p (t/char-of "abc")
       "b")
    {:consumed true, :value (c "b")}

    (p (t/char-of "abc")
       "c")
    {:consumed true, :value (c "c")}

    (p (t/char-of "abc")
       "d")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected \"d\""
                              "expecting (char-of \"abc\")"]}

    (p (t/char-of "abc")
       "")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected end of input"
                              "expecting (char-of \"abc\")"]}

    (p (t/char-of "a")
       "d")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected \"d\""
                              "expecting \"a\""]}

    (p (t/char-of "a")
       "")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected end of input"
                              "expecting \"a\""]}

    ))

(deftest char-of-not-t
  (test/are [expr result] (= result expr)

    (p (t/char-of-not "abc")
       "x")
    {:consumed true, :value (c "x")}

    (p (t/char-of-not "abc")
       "a")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected \"a\""
                              "expecting (char-of-not \"abc\")"]}

    (p (t/char-of-not "abc")
       "")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected end of input"
                              "expecting (char-of-not \"abc\")"]}

    ))

(deftest matches-t
  (test/are [expr result] (= result expr)

    (p (p/many* (t/matches #"[a-z]"))
       "abc")
    {:consumed true, :value (seq "abc")}

    (p (t/matches #"[a-z]")
       "A")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected \"A\""
                              "expecting (matches #\"[a-z]\")"]}

    (p (t/matches #"[a-z]")
       "")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected end of input"
                              "expecting (matches #\"[a-z]\")"]}

    (p (t/matches #"[a-z]" "a to z")
       "A")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected \"A\""
                              "expecting a to z"]}

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest whitespace-t
  (test/are [expr result] (= result expr)

    (p (p/many* t/whitespace)
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

(deftest skip-space-t
  (test/are [expr result] (= result expr)

    (p t/skip-space*
       " \t\r\n")
    {:consumed true, :value nil}

    (p t/skip-space*
       "a")
    {:consumed false, :value nil}

    (p t/skip-space*
       "")
    {:consumed false, :value nil}

    (p t/skip-space+
       " \t\r\n")
    {:consumed true, :value nil}

    (p t/skip-space+
       "a")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected \"a\""
                              "expecting whitespace character"]}

    (p t/skip-space+
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

    (p (p/many* t/upper)
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

    (p (p/many* t/lower)
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

    (p (p/many* t/numeric)
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

(deftest alpha-num-t
  (test/are [expr result] (= result expr)

    (p (p/many* t/alpha-num)
       "12345abcABC")
    {:consumed true, :value (seq "12345abcABC")}

    (p t/alpha-num
       "-")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected \"-\""
                              "expecting alphanumeric character"]}

    (p t/alpha-num
       "")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected end of input"
                              "expecting alphanumeric character"]}

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest substr-t
  (test/are [expr result] (= result expr)

    (p (t/substr "abc")
       "abc")
    {:consumed true, :value "abc"}

    (p (t/substr "abc")
       "ab")
    {:consumed true, :error ["at line 1, column 3:"
                             "unexpected end of input"
                             "expecting \"c\" in (substr \"abc\")"]}

    (p (t/substr "abc")
       "")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected end of input"
                              "expecting \"a\" in (substr \"abc\")"]}

    (p (t/substr "abc")
       "abx")
    {:consumed true, :error ["at line 1, column 3:"
                             "unexpected \"x\""
                             "expecting \"c\" in (substr \"abc\")"]}

    (p (t/substr "abc")
       "xyz")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected \"x\""
                              "expecting \"a\" in (substr \"abc\")"]}

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest to-str-t
  (test/are [expr result] (= result expr)

    (p (t/to-str (t/char-of "abc"))
       "abc")
    {:consumed true, :value "a"}

    (p (t/to-str (p/sequence [(p/many+ (t/char-of "abc"))
                              (p/many+ (t/char-of "123"))]))
       "abc123")
    {:consumed true, :value "abc123"}

    (p (t/to-str (p/many* (t/char-of "abc")))
       "123")
    {:consumed false, :value ""}

    (p (t/to-str (t/char-of "abc"))
       "123")
    {:consumed false, :error ["at line 1, column 1:"
                              "unexpected \"1\""
                              "expecting (char-of \"abc\")"]}

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
