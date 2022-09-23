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
    (cond-> result
      (p/error? result) (assoc :value :<NA> :message (string/split-lines (str (:error result))))
      :then,,,,,,,,,,,, (select-keys [:value :consumed :message]))))

(defn- c
  "Cross-platform char."
  [s]
  #?(:cljs s, :clj (first s)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest char-t
  (test/are [expr result] (= result expr)

    (p (t/char \a)
       "a")
    {:value (c "a"), :consumed true}

    (p (t/char \a)
       "b")
    {:value :<NA>, :consumed false, :message ["(line 1, column 1):"
                                              "unexpected \"b\""
                                              "expecting \"a\""]}

    (p (t/char \a)
       "")
    {:value :<NA>, :consumed false, :message ["(line 1, column 1):"
                                              "unexpected end of input"
                                              "expecting \"a\""]}

    ))

(deftest any-char-t
  (test/are [expr result] (= result expr)

    (p t/any-char
       "a")
    {:value (c "a"), :consumed true}

    (p t/any-char
       "")
    {:value :<NA>, :consumed false, :message ["(line 1, column 1):"
                                              "unexpected end of input"]}

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest one-of-t
  (test/are [expr result] (= result expr)

    (p (t/one-of "abc")
       "a")
    {:value (c "a"), :consumed true}

    (p (t/one-of "abc")
       "b")
    {:value (c "b"), :consumed true}

    (p (t/one-of "abc")
       "c")
    {:value (c "c"), :consumed true}

    (p (t/one-of "abc")
       "d")
    {:value :<NA>, :consumed false, :message ["(line 1, column 1):"
                                              "unexpected \"d\""]}

    (p (t/one-of "abc")
       "")
    {:value :<NA>, :consumed false, :message ["(line 1, column 1):"
                                              "unexpected end of input"]}

    ))

(deftest none-of-t
  (test/are [expr result] (= result expr)

    (p (t/none-of "abc")
       "x")
    {:value (c "x"), :consumed true}

    (p (t/none-of "abc")
       "a")
    {:value :<NA>, :consumed false, :message ["(line 1, column 1):"
                                              "unexpected \"a\""]}

    (p (t/none-of "abc")
       "")
    {:value :<NA>, :consumed false, :message ["(line 1, column 1):"
                                              "unexpected end of input"]}

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
