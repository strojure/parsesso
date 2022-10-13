(ns strojure.parsesso.char-test
  (:require [clojure.string :as string]
            [clojure.test :as test :refer [deftest testing]]
            [strojure.parsesso.char :as char]
            [strojure.parsesso.parser :as p]))

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

(deftest is-t
  (testing "default matching"
    (test/are [expr result] (= result expr)

      (p (char/is "abc")
         "a")
      {:consumed true, :value (c "a")}

      (p (char/is "abc")
         "b")
      {:consumed true, :value (c "b")}

      (p (char/is "abc")
         "c")
      {:consumed true, :value (c "c")}

      (p (char/is "abc")
         "d")
      {:consumed false, :error ["error at line 1, column 1:"
                                "unexpected \"d\""
                                "expecting \"abc\" character"]}

      (p (char/is "abc")
         "")
      {:consumed false, :error ["error at line 1, column 1:"
                                "unexpected end of input"
                                "expecting \"abc\" character"]}

      (p (char/is "a")
         "d")
      {:consumed false, :error ["error at line 1, column 1:"
                                "unexpected \"d\""
                                "expecting \"a\" character"]}

      (p (char/is "a")
         "")
      {:consumed false, :error ["error at line 1, column 1:"
                                "unexpected end of input"
                                "expecting \"a\" character"]}

      ))

  (testing "case insensitive matching"
    (test/are [expr result] (= result expr)

      (p (char/is "abc" :ic)
         "a")
      {:consumed true, :value (c "a")}

      (p (char/is "abc" :ic)
         "A")
      {:consumed true, :value (c "A")}

      (p (char/is "ABC" :ic)
         "a")
      {:consumed true, :value (c "a")}

      (p (char/is "abc" :ic)
         "d")
      {:consumed false, :error ["error at line 1, column 1:"
                                "unexpected \"d\""
                                "expecting \"abc\" character"]}

      )))

(deftest is-not-t
  (testing "default matching"
    (test/are [expr result] (= result expr)

      (p (char/is-not "abc")
         "x")
      {:consumed true, :value (c "x")}

      (p (char/is-not "abc")
         "a")
      {:consumed false, :error ["error at line 1, column 1:"
                                "unexpected \"a\""
                                "expecting not \"abc\" character"]}

      (p (char/is-not "abc")
         "")
      {:consumed false, :error ["error at line 1, column 1:"
                                "unexpected end of input"
                                "expecting not \"abc\" character"]}

      (p (char/is-not "a")
         "a")
      {:consumed false, :error ["error at line 1, column 1:"
                                "unexpected \"a\""
                                "expecting not \"a\" character"]}

      (p (char/is-not "a")
         "")
      {:consumed false, :error ["error at line 1, column 1:"
                                "unexpected end of input"
                                "expecting not \"a\" character"]}

      ))

  (testing "case insensitive matching"
    (test/are [expr result] (= result expr)

      (p (char/is-not "abc" :ic)
         "x")
      {:consumed true, :value (c "x")}

      (p (char/is-not "abc" :ic)
         "a")
      {:consumed false, :error ["error at line 1, column 1:"
                                "unexpected \"a\""
                                "expecting not \"abc\" character"]}

      (p (char/is-not "abc" :ic)
         "A")
      {:consumed false, :error ["error at line 1, column 1:"
                                "unexpected \"A\""
                                "expecting not \"abc\" character"]}

      (p (char/is-not "a" :ic)
         "a")
      {:consumed false, :error ["error at line 1, column 1:"
                                "unexpected \"a\""
                                "expecting not \"a\" character"]}

      (p (char/is-not "a" :ic)
         "A")
      {:consumed false, :error ["error at line 1, column 1:"
                                "unexpected \"A\""
                                "expecting not \"a\" character"]}

      )))

(deftest regex-t
  (test/are [expr result] (= result expr)

    (p (p/many0 (char/regex #"[a-z]"))
       "abc")
    {:consumed true, :value (seq "abc")}

    (p (char/regex #"[a-z]")
       "A")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected \"A\""
                              "expecting character matching regex #\"[a-z]\""]}

    (p (char/regex #"[a-z]")
       "")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected end of input"
                              "expecting character matching regex #\"[a-z]\""]}

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest letter?-t
  (test/are [expr result] (= result expr)

    (p char/letter?
       "a")
    {:consumed true, :value (c "a")}

    (p char/letter?
       "1")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected \"1\""
                              "expecting ascii letter"]}

    (p char/letter?
       "")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected end of input"
                              "expecting ascii letter"]}

    ))

(deftest upper?-t
  (test/are [expr result] (= result expr)

    (p (p/many0 char/upper?)
       "ABC")
    {:consumed true, :value (seq "ABC")}

    (p char/upper?
       "a")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected \"a\""
                              "expecting upper-case ascii letter"]}

    (p char/upper?
       "")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected end of input"
                              "expecting upper-case ascii letter"]}

    ))

(deftest lower?-t
  (test/are [expr result] (= result expr)

    (p (p/many0 char/lower?)
       "abc")
    {:consumed true, :value (seq "abc")}

    (p char/lower?
       "A")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected \"A\""
                              "expecting lower-case ascii letter"]}

    (p char/lower?
       "")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected end of input"
                              "expecting lower-case ascii letter"]}

    ))

(deftest number?-t
  (test/are [expr result] (= result expr)

    (p (p/many0 char/number?)
       "01234567890")
    {:consumed true, :value (seq "01234567890")}

    (p char/number?
       "a")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected \"a\""
                              "expecting ascii number"]}

    (p char/number?
       "")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected end of input"
                              "expecting ascii number"]}

    ))

(deftest letter-or-number?-t
  (test/are [expr result] (= result expr)

    (p (p/many0 char/letter-or-number?)
       "12345abcABC")
    {:consumed true, :value (seq "12345abcABC")}

    (p char/letter-or-number?
       "-")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected \"-\""
                              "expecting ascii letter or number"]}

    (p char/letter-or-number?
       "")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected end of input"
                              "expecting ascii letter or number"]}

    ))

(deftest white?-t
  (test/are [expr result] (= result expr)

    (p (p/many0 char/white?)
       " \t\r\n")
    {:consumed true, :value (seq " \t\r\n")}

    (p char/white?
       "a")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected \"a\""
                              "expecting whitespace character"]}

    (p char/white?
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
                             "expecting \"\\n\" character"]}

    (p char/newline
       "\r")
    {:consumed true, :error ["error at line 1, column 2:"
                             "unexpected end of input"
                             "expecting \"\\n\" character"]}

    (p char/newline
       "a")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected \"a\""
                              "expecting \"\\n\" character or \"\\r\" character"]}

    (p char/newline
       "")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected end of input"
                              "expecting \"\\n\" character or \"\\r\" character"]}

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest str*-t
  (test/are [expr result] (= result expr)

    (p (-> (char/is "abc")
           (p/using char/str*))
       "abc")
    {:consumed true, :value "a"}

    (p (-> (p/tuple (p/many1 (char/is "abc"))
                    (p/many1 (char/is "123")))
           (p/using char/str*))
       "abc123")
    {:consumed true, :value "abc123"}

    (p (-> (p/many0 (char/is "abc"))
           (p/using char/str*))
       "123")
    {:consumed false, :value ""}

    (p (-> (char/is "abc")
           (p/using char/str*))
       "123")
    {:consumed false, :error ["error at line 1, column 1:"
                              "unexpected \"1\""
                              "expecting \"abc\" character"]}

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
