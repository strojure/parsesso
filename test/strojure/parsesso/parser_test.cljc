(ns strojure.parsesso.parser-test
  (:require [clojure.string :as string]
            [clojure.test :as test :refer [deftest testing]]
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

(defn- tok
  [& cs]
  (p/token (set cs)))

(defn- fail-consumed
  "Returns parser which fails when `p` is successfully consumed."
  [parser]
  (p/alt (p/for [x parser] (p/fail (str "Test failure after parsing " x)))
         parser))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest result-t
  (test/are [expr result] (= result expr)

    (p (p/result :A)
       [])
    {:consumed false, :value :A}

    (p (p/result :A)
       [:B])
    {:consumed false, :value :A}

    (p (fail-consumed (p/result :A))
       [])
    {:consumed false, :value :A}

    ))

(deftest fail-t
  (test/are [expr result] (= result expr)

    (p (p/fail "Test failure")
       [])
    {:consumed false, :error ["error at index 0:"
                              "Test failure"]}

    (p (p/fail "Test failure")
       [:A])
    {:consumed false, :error ["error at index 0:"
                              "Test failure"]}

    (p (p/fail nil)
       [])
    {:consumed false, :error ["error at index 0:"]}

    (p (p/fail)
       [])
    {:consumed false, :error ["error at index 0:"]}

    ))

(deftest fail-unexpected-t
  (test/are [expr result] (= result expr)

    (p (p/fail-unexpected "Boom")
       [])
    {:consumed false, :error ["error at index 0:"
                              "unexpected Boom"]}

    (p (-> (p/fail-unexpected "Boom")
           (p/expecting "description"))
       [])
    {:consumed false, :error ["error at index 0:"
                              "unexpected Boom"
                              "expecting description"]}

    (p (p/fail-unexpected nil)
       [])
    {:consumed false, :error ["error at index 0:"
                              "unexpected nil"]}

    ))

(deftest expecting-t
  (test/are [expr result] (= result expr)

    (p (-> (p/fail "Test failure")
           (p/expecting "expectation"))
       [])
    {:consumed false, :error ["error at index 0:"
                              "expecting expectation"
                              "Test failure"]}

    (p (-> (p/fail "Test failure")
           (p/expecting (delay "expectation")))
       [])
    {:consumed false, :error ["error at index 0:"
                              "expecting expectation"
                              "Test failure"]}

    (p (-> (p/fail "Test failure")
           (p/expecting nil))
       [])
    {:consumed false, :error ["error at index 0:"
                              "Test failure"]}

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest bind-t
  (test/are [expr result] (= result expr)

    (p (p/bind (tok :A) p/result)
       [:A])
    {:consumed true, :value :A}

    (p (p/bind (tok :A) (fn [_] (p/fail "Oops")))
       [:A])
    {:consumed true, :error ["error at index 1:"
                             "Oops"]}

    (p (p/bind (tok :A) p/result)
       [:B])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :B"]}

    (p (p/bind (tok :A) (fn [_] (p/fail "Oops")))
       [:B])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :B"]}

    (p (p/bind (tok :A) (fn [_] (tok :B)))
       [:A :B])
    {:consumed true, :value :B}

    (p (p/bind (tok :A) (fn [_] (tok :B)))
       [:B :A])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :B"]}

    (p (p/bind (tok :A) (fn [_] (tok :B)))
       [:A :A])
    {:consumed true, :error ["error at index 1:"
                             "unexpected :A"]}

    ))

(deftest after-t
  (test/are [expr result] (= result expr)

    (p (p/after (tok :A) (tok :B))
       [:A :B])
    {:consumed true, :value :B}

    (p (p/after (tok :A) (tok :B))
       [:A :A])
    {:consumed true, :error ["error at index 1:"
                             "unexpected :A"]}

    (p (p/after (tok :A) (tok :B))
       [:A])
    {:consumed true, :error ["error at index 1:"
                             "unexpected end of input"]}

    (p (p/after (fail-consumed (tok :A)) (tok :B))
       [:A :B])
    {:consumed true, :error ["error at index 1:"
                             "Test failure after parsing :A"]}

    (p (p/after (tok :A) (fail-consumed (tok :B)))
       [:A :B])
    {:consumed true, :error ["error at index 2:"
                             "Test failure after parsing :B"]}

    (p (p/after (tok :A) (tok :B) (tok :C))
       [:A :B :C])
    {:consumed true, :value :C}

    ))

(deftest value-t
  (test/are [expr result] (= result expr)

    (p (p/value (tok :A) name)
       [:A])
    {:consumed true, :value "A"}

    (p (p/value (p/token number?) inc inc)
       [1])
    {:consumed true, :value 3}

    (p (p/value (p/token number?) inc inc inc str)
       [1])
    {:consumed true, :value "4"}

    (p (p/value (tok :A) name)
       [:B])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :B"]}

    (p (p/value (tok :A) name)
       [])
    {:consumed false, :error ["error at index 0:"
                              "unexpected end of input"]}

    (p (p/value (fail-consumed (tok :A)) name)
       [:A])
    {:consumed true, :error ["error at index 1:"
                             "Test failure after parsing :A"]}

    (p (p/value (fail-consumed (tok :A)) name)
       [:B])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :B"]}

    (p (p/value (fail-consumed (tok :A)) name)
       [])
    {:consumed false, :error ["error at index 0:"
                              "unexpected end of input"]}

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest maybe-t
  (test/are [expr result] (= result expr)

    (p (p/maybe (tok :A))
       [:A])
    {:consumed true, :value :A}

    (p (p/maybe (tok :A))
       [:B])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :B"]}

    (p (p/maybe (tok :A))
       [])
    {:consumed false, :error ["error at index 0:"
                              "unexpected end of input"]}

    (p (p/maybe (fail-consumed (tok :A)))
       [:A])
    {:consumed false, :error ["error at index 1:"
                              "Test failure after parsing :A"]}

    (p (p/maybe (fail-consumed (tok :A)))
       [:B])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :B"]}

    (p (p/maybe (fail-consumed (tok :A)))
       [])
    {:consumed false, :error ["error at index 0:"
                              "unexpected end of input"]}

    ))

(deftest look-ahead-t
  (test/are [expr result] (= result expr)

    (p (p/look-ahead (tok :A))
       [:A])
    {:consumed false, :value :A}

    (p (p/look-ahead (tok :A))
       [:B])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :B"]}

    (p (p/look-ahead (tok :A))
       [])
    {:consumed false, :error ["error at index 0:"
                              "unexpected end of input"]}

    (p (p/look-ahead (fail-consumed (tok :A)))
       [:A])
    {:consumed true, :error ["error at index 1:"
                             "Test failure after parsing :A"]}

    (p (p/look-ahead (fail-consumed (tok :A)))
       [:B])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :B"]}

    (p (p/look-ahead (fail-consumed (tok :A)))
       [])
    {:consumed false, :error ["error at index 0:"
                              "unexpected end of input"]}

    ))

(deftest not-followed-by-t
  (testing "not-followed-by [p q]"
    (test/are [expr result] (= result expr)

      (p (p/not-followed-by (p/result :X)
                            (tok :A))
         [:B])
      {:consumed false, :value :X}

      (p (p/not-followed-by (tok :X)
                            (tok :A))
         [:X :B])
      {:consumed true, :value :X}

      (p (p/not-followed-by (p/result :X)
                            (p/after (tok :A) (tok :B)))
         [:A :A])
      {:consumed false, :value :X}

      (p (p/not-followed-by (tok :X)
                            (p/after (tok :A) (tok :B)))
         [:X :A :A])
      {:consumed true, :value :X}

      (p (p/not-followed-by (p/result :X)
                            (tok :A))
         [])
      {:consumed false, :value :X}

      (p (p/not-followed-by (tok :X)
                            (tok :A))
         [:X])
      {:consumed true, :value :X}

      (p (p/not-followed-by (p/result :X)
                            p/any-token)
         [])
      {:consumed false, :value :X}

      (p (p/not-followed-by (tok :X)
                            p/any-token)
         [:X])
      {:consumed true, :value :X}

      (p (p/not-followed-by (p/result :X)
                            (tok :A))
         [:A])
      {:consumed false, :error ["error at index 0:"
                                "unexpected :A"]}

      (p (p/not-followed-by (tok :X)
                            (tok :A))
         [:X :A])
      {:consumed true, :error ["error at index 1:"
                               "unexpected :A"]}

      (p (p/not-followed-by (p/result :X)
                            (p/after (tok :A) (tok :B)))
         [:A :B])
      {:consumed false, :error ["error at index 0:"
                                "unexpected :A"]}

      (p (p/not-followed-by (tok :X)
                            (p/after (tok :A) (tok :B)))
         [:X :A :B])
      {:consumed true, :error ["error at index 1:"
                               "unexpected :A"]}

      (p (p/not-followed-by (p/result :X)
                            p/any-token)
         [:A])
      {:consumed false, :error ["error at index 0:"
                                "unexpected :A"]}

      (p (p/not-followed-by (tok :X)
                            p/any-token)
         [:X :A])
      {:consumed true, :error ["error at index 1:"
                               "unexpected :A"]}

      (p (p/not-followed-by (p/result :X)
                            (p/eof))
         [])
      {:consumed false, :error ["error at index 0:"
                                "unexpected end of input"]}

      (p (p/not-followed-by (tok :X)
                            (p/eof))
         [:X])
      {:consumed true, :error ["error at index 1:"
                               "unexpected end of input"]}

      ))

  (testing "not-followed-by [q]"
    (test/are [expr result] (= result expr)

      (p (p/not-followed-by (tok :A))
         [:B])
      {:consumed false, :value nil}

      (p (p/not-followed-by (p/after (tok :A) (tok :B)))
         [:A :A])
      {:consumed false, :value nil}

      (p (p/not-followed-by (tok :A))
         [])
      {:consumed false, :value nil}

      (p (p/not-followed-by p/any-token)
         [])
      {:consumed false, :value nil}

      (p (p/not-followed-by (tok :A))
         [:A])
      {:consumed false, :error ["error at index 0:"
                                "unexpected :A"]}

      (p (p/not-followed-by (p/after (tok :A) (tok :B)))
         [:A :B])
      {:consumed false, :error ["error at index 0:"
                                "unexpected :A"]}

      (p (p/not-followed-by p/any-token)
         [:A])
      {:consumed false, :error ["error at index 0:"
                                "unexpected :A"]}

      (p (p/not-followed-by p/eof)
         [])
      {:consumed false, :error ["error at index 0:"
                                "unexpected end of input"]}

      )))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest *many-t
  (test/are [expr result] (= result expr)

    (p (p/*many (tok :A :B :C))
       [:A :B :C :D :E :F])
    {:consumed true, :value [:A :B :C]}

    (p (p/*many (fail-consumed (tok :A :B :C)))
       [:A :B :C :D :E :F])
    {:consumed true, :error ["error at index 1:"
                             "Test failure after parsing :A"]}

    (p (p/*many (tok :D :E :F))
       [:A :B :C :D :E :F])
    {:consumed false, :value nil}

    (p (p/*many (tok :A :B :C))
       [])
    {:consumed false, :value nil}

    (p (p/*many (tok :A))
       (repeat 10000 :A))
    {:consumed true, :value (repeat 10000 :A)}

    ))

(deftest +many-t
  (test/are [expr result] (= result expr)

    (p (p/+many (tok :A :B :C))
       [:A :B :C :D :E :F])
    {:consumed true, :value [:A :B :C]}

    (p (p/+many (tok :D :E :F))
       [:A :B :C :D :E :F])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :A"]}

    (p (p/+many (tok :A :B :C))
       [])
    {:consumed false, :error ["error at index 0:"
                              "unexpected end of input"]}

    (p (p/+many (tok :A))
       (repeat 10000 :A))
    {:consumed true, :value (repeat 10000 :A)}

    ))

(deftest *skip-t
  (test/are [expr result] (= result expr)

    (p (p/*skip (tok :A))
       [:A :A :A :B :B :B])
    {:consumed true, :value nil}

    (p (p/*skip (fail-consumed (tok :A)))
       [:A :A :A :B :B :B])
    {:consumed true, :error ["error at index 1:"
                             "Test failure after parsing :A"]}

    (p (p/*skip (tok :A))
       [:B :B :B])
    {:consumed false, :value nil}

    (p (p/*skip (tok :A))
       [])
    {:consumed false, :value nil}

    )
  )

(deftest +skip-t
  (test/are [expr result] (= result expr)

    (p (p/+skip (tok :A))
       [:A :A :A :B :B :B])
    {:consumed true, :value nil}

    (p (p/+skip (tok :A))
       [:B :B :B])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :B"]}

    (p (p/+skip (tok :A))
       [])
    {:consumed false, :error ["error at index 0:"
                              "unexpected end of input"]}

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest token-t
  (test/are [expr result] (= result expr)

    (p (p/token #{:A})
       [:A])
    {:consumed true, :value :A}

    (p (p/token #{:A})
       [:B])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :B"]}

    (p (p/token #{:A})
       [])
    {:consumed false, :error ["error at index 0:"
                              "unexpected end of input"]}

    (p (fail-consumed (p/token #{:A}))
       [:A])
    {:consumed true, :error ["error at index 1:"
                             "Test failure after parsing :A"]}

    (p (fail-consumed (p/token #{:A}))
       [:B])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :B"]}

    (p (fail-consumed (p/token #{:A}))
       [])
    {:consumed false, :error ["error at index 0:"
                              "unexpected end of input"]}

    ))

(deftest token-not-t
  (test/are [expr result] (= result expr)

    (p (p/token-not #{:A})
       [:B])
    {:consumed true, :value :B}

    (p (p/token-not #{:A})
       [:A])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :A"]}

    (p (p/token-not #{:A})
       [])
    {:consumed false, :error ["error at index 0:"
                              "unexpected end of input"]}

    (p (fail-consumed (p/token-not #{:A}))
       [:B])
    {:consumed true, :error ["error at index 1:"
                             "Test failure after parsing :B"]}

    (p (fail-consumed (p/token-not #{:A}))
       [:A])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :A"]}

    (p (fail-consumed (p/token-not #{:A}))
       [])
    {:consumed false, :error ["error at index 0:"
                              "unexpected end of input"]}

    ))

(deftest word-t
  (testing "default matching"
    (test/are [expr result] (= result expr)

      (p (p/word [:A :B :C])
         [:A :B :C])
      {:consumed true, :value [:A :B :C]}

      (p (p/word [:A :B :C])
         [:A :B])
      {:consumed true, :error ["error at index 0:"
                               "unexpected end of input"
                               "expecting [:A :B :C]"]}

      (p (p/word [:A :B :C])
         [])
      {:consumed false, :error ["error at index 0:"
                                "unexpected end of input"
                                "expecting [:A :B :C]"]}

      (p (p/word [:A :B :C])
         [:A :B :X])
      {:consumed true, :error ["error at index 0:"
                               "unexpected :X"
                               "expecting [:A :B :C]"]}

      (p (p/word [:A :B :C])
         [:X :Y :Z])
      {:consumed false, :error ["error at index 0:"
                                "unexpected :X"
                                "expecting [:A :B :C]"]}

      (p (p/word [:ns/A :ns/B :ns/C]
                 (fn [w t] (= (name w) (name t))))
         [:A :B :C])
      {:consumed true, :value [:ns/A :ns/B :ns/C]}

      ))

  (testing "case insensitive matching"
    (test/are [expr result] (= result expr)

      (p (p/word "abc" :ic)
         "abc")
      {:consumed true, :value "abc"}

      (p (p/word "abc" :ic)
         "ABC")
      {:consumed true, :value "abc"}

      (p (p/word "ABC" :ic)
         "abc")
      {:consumed true, :value "ABC"}

      (p (p/word "abc" :ic)
         "abd")
      {:consumed true, :error ["error at line 1, column 1:"
                               "unexpected \"d\""
                               "expecting \"abc\""]}

      (p (p/word "abc" :ic)
         "ab")
      {:consumed true, :error ["error at line 1, column 1:"
                               "unexpected end of input"
                               "expecting \"abc\""]}

      )))

(deftest any-token-t
  (test/are [expr result] (= result expr)

    (p p/any-token
       [:A])
    {:consumed true, :value :A}

    (p p/any-token
       [])
    {:consumed false, :error ["error at index 0:"
                              "unexpected end of input"]}

    ))

(deftest eof-t
  (test/are [expr result] (= result expr)

    (p p/eof
       [])
    {:consumed false, :value nil}

    (p (p/eof :ok)
       [])
    {:consumed false, :value :ok}

    (p p/eof
       [:A])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :A"
                              "expecting end of input"]}

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest group*-t
  (test/are [expr result] (= result expr)

    (p (p/group* [(tok :A) (tok :B) (tok :C)])
       [:A :B :C])
    '{:consumed true, :value (:A :B :C)}

    (p (p/group* [(tok :A) (tok :B) (tok :C)])
       [:B :C])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :B"]}

    (p (p/group* [(fail-consumed (tok :A)) (tok :B) (tok :C)])
       [:A :B :C])
    {:consumed true, :error ["error at index 1:"
                             "Test failure after parsing :A"]}

    (p (p/group* [])
       [:A :B :C])
    {:consumed false, :value nil}

    (p (p/group* nil)
       [:A :B :C])
    {:consumed false, :value nil}

    ))

(deftest group-t
  (test/are [expr result] (= result expr)

    (p (p/group (tok :A) (tok :B) (tok :C))
       [:A :B :C])
    '{:consumed true, :value (:A :B :C)}

    (p (p/group (tok :A) (tok :B) (tok :C))
       [:B :C])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :B"]}

    (p (p/group (fail-consumed (tok :A)) (tok :B) (tok :C))
       [:A :B :C])
    {:consumed true, :error ["error at index 1:"
                             "Test failure after parsing :A"]}

    ))

(deftest alt-t
  (test/are [expr result] (= result expr)

    (p (p/alt (tok :A)
              (tok :B))
       [:A])
    {:consumed true, :value :A}

    (p (p/alt (tok :A)
              (tok :B))
       [:B])
    {:consumed true, :value :B}

    (p (p/alt (tok :A)
              (tok :B))
       [:C])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :C"]}

    (p (p/alt (tok :A)
              (tok :B))
       [])
    {:consumed false, :error ["error at index 0:"
                              "unexpected end of input"]}

    (p (p/alt (fail-consumed (tok :A))
              (tok :B))
       [:A])
    {:consumed true, :error ["error at index 1:"
                             "Test failure after parsing :A"]}

    (p (p/alt (fail-consumed (tok :A))
              (tok :B))
       [:B])
    {:consumed true, :value :B}

    (p (p/alt (fail-consumed (tok :A))
              (tok :B))
       [:C])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :C"]}

    (p (p/alt (fail-consumed (tok :A))
              (tok :B))
       [])
    {:consumed false, :error ["error at index 0:"
                              "unexpected end of input"]}

    (p (p/alt (tok :A)
              (fail-consumed (tok :B)))
       [:A])
    {:consumed true, :value :A}

    (p (p/alt (tok :A)
              (fail-consumed (tok :B)))
       [:B])
    {:consumed true, :error ["error at index 1:"
                             "Test failure after parsing :B"]}

    (p (p/alt (tok :A)
              (fail-consumed (tok :B)))
       [:C])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :C"]}

    (p (p/alt (tok :A)
              (fail-consumed (tok :B)))
       [])
    {:consumed false, :error ["error at index 0:"
                              "unexpected end of input"]}

    (p (p/alt (p/expecting (tok :A) :A)
              (p/expecting (tok :B) :B))
       [:C])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :C"
                              "expecting :A or :B"]}

    ))

(deftest option-t
  (testing "The `option` without default."
    (test/are [expr result] (= result expr)

      (p (p/option (tok :A))
         [:A])
      {:consumed true, :value :A}

      (p (p/option (tok :A))
         [:B])
      {:consumed false, :value nil}

      (p (p/option (tok :A))
         [])
      {:consumed false, :value nil}

      (p (p/option (fail-consumed (tok :A)))
         [:A])
      {:consumed true, :error ["error at index 1:"
                               "Test failure after parsing :A"]}

      (p (p/option (fail-consumed (tok :A)))
         [:B])
      {:consumed false, :value nil}

      (p (p/option (fail-consumed (tok :A)))
         [])
      {:consumed false, :value nil}

      ))

  (testing "The `option` with default value."
    (test/are [expr result] (= result expr)

      (p (p/option (tok :A) :X)
         [:A])
      {:consumed true, :value :A}

      (p (p/option (tok :A) :X)
         [:B])
      {:consumed false, :value :X}

      (p (p/option (tok :A) :X)
         [])
      {:consumed false, :value :X}

      (p (p/option (fail-consumed (tok :A)) :X)
         [:A])
      {:consumed true, :error ["error at index 1:"
                               "Test failure after parsing :A"]}

      (p (p/option (fail-consumed (tok :A)) :X)
         [:B])
      {:consumed false, :value :X}

      (p (p/option (fail-consumed (tok :A)) :X)
         [])
      {:consumed false, :value :X}

      )))

(deftest between-t
  (test/are [expr result] (= result expr)

    (p (p/between (tok :A) (tok :L) (tok :R))
       [:L :A :R])
    {:consumed true, :value :A}

    (p (p/between (tok :A) (tok :L) (tok :R))
       [:R :A :L])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :R"]}

    (p (p/between (tok :A) (tok :L) (tok :R))
       [:L :A])
    {:consumed true, :error ["error at index 2:"
                             "unexpected end of input"]}

    (p (p/between (tok :A) (tok :L) (tok :R))
       [:A :R])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :A"]}

    (p (p/between (tok :A) (tok :L) (tok :R))
       [:A])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :A"]}

    (p (p/between (tok :A) (tok :L) (tok :R))
       [])
    {:consumed false, :error ["error at index 0:"
                              "unexpected end of input"]}

    (p (p/between (tok :A) (tok :I))
       [:I :A :I])
    {:consumed true, :value :A}

    (p (p/between (tok :A) (tok :I))
       [:I :A])
    {:consumed true, :error ["error at index 2:"
                             "unexpected end of input"]}

    (p (p/between (tok :A) (tok :I))
       [:A :I])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :A"]}

    (p (p/between (tok :A) (tok :I))
       [:A])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :A"]}

    (p (p/between (tok :A) (tok :I))
       [])
    {:consumed false, :error ["error at index 0:"
                              "unexpected end of input"]}

    ))

(deftest times-t
  (test/are [expr result] (= result expr)

    (p (p/times 3 (tok :A1 :A2 :A3))
       [:A1 :A2 :A3])
    {:consumed true, :value '(:A1 :A2 :A3)}

    (p (p/times 3 (tok :A1 :A2 :A3))
       [:A1 :A2 :A3 :A4])
    {:consumed true, :value '(:A1 :A2 :A3)}

    (p (p/times 3 (tok :A1 :A2 :A3))
       [:A1 :A2 :A3 :B])
    {:consumed true, :value '(:A1 :A2 :A3)}

    (p (p/times 3 (tok :A1 :A2 :A3))
       [:A1 :A2])
    {:consumed true, :error ["error at index 2:"
                             "unexpected end of input"]}

    (p (p/times 3 (tok :A1 :A2 :A3))
       [:A1 :A2 :B])
    {:consumed true, :error ["error at index 2:"
                             "unexpected :B"]}

    (p (p/times 3 (tok :A1 :A2 :A3))
       [:B :A1 :A2 :A3])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :B"]}

    (p (p/times 3 (tok :A1 :A2 :A3))
       [:B :A1])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :B"]}

    (p (p/times 3 (tok :A1 :A2 :A3))
       [])
    {:consumed false, :error ["error at index 0:"
                              "unexpected end of input"]}

    (p (p/times 0 (tok :A1 :A2 :A3))
       [:A1 :A2 :A3])
    {:consumed false, :value nil}

    (p (p/times -3 (tok :A1 :A2 :A3))
       [:A1 :A2 :A3])
    {:consumed false, :value nil}

    ))

(deftest *many-till-t
  (test/are [expr result] (= result expr)

    (p (p/*many-till (tok :A1 :A2 :A3)
                     (tok :END))
       [:A1 :A2 :A3 :END])
    {:consumed true, :value '(:A1 :A2 :A3)}

    (p (p/*many-till (tok :A1 :A2 :A3)
                     (tok :END))
       [:A1 :A2 :A3 :B :END])
    {:consumed true, :error ["error at index 3:"
                             "unexpected :B"]}

    (p (p/*many-till (tok :A1 :A2 :A3)
                     (tok :END))
       [:B :END])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :B"]}

    (p (p/*many-till (tok :A1 :A2 :A3)
                     (tok :END))
       [:A1 :A2 :A3])
    {:consumed true, :error ["error at index 3:"
                             "unexpected end of input"]}

    (p (p/*many-till (fail-consumed (tok :A1 :A2 :A3))
                     (tok :END))
       [:A1 :A2 :A3 :END])
    {:consumed true, :error ["error at index 1:"
                             "Test failure after parsing :A1"]}

    (p (p/*many-till (tok :A1 :A2 :A3)
                     (tok :END))
       [:END])
    {:consumed true, :value nil}

    (p (p/*many-till (p/alt (tok :A1 :A2 :A3)
                            (p/*many-till (tok :B1 :B2 :B3)
                                          (tok :END)))
                     (tok :END))
       [:A1 :A2 :A3 :B1 :B2 :B3 :END :A1 :A2 :A3 :END])
    {:consumed true, :value '(:A1 :A2 :A3 (:B1 :B2 :B3) :A1 :A2 :A3)}

    (p (p/*many-till (tok :A1 :A2 :A3)
                     (tok :END))
       (concat (take 10000 (cycle [:A1 :A2 :A3])) [:END]))
    {:consumed true, :value (take 10000 (cycle [:A1 :A2 :A3]))}

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest +sep-by-t
  (test/are [expr result] (= result expr)

    (p (p/+sep-by (tok :A) (tok :S))
       [:A :S :A :S :A])
    {:consumed true, :value '(:A :A :A)}

    (p (p/+sep-by (tok :A) (tok :S))
       [:A :S :A :S :A :S])
    {:consumed true, :error ["error at index 6:"
                             "unexpected end of input"]}

    (p (p/+sep-by (tok :A) (tok :S))
       [:A :S :A :S :A :B])
    {:consumed true, :value '(:A :A :A)}

    (p (p/+sep-by (tok :A) (tok :S))
       [])
    {:consumed false, :error ["error at index 0:"
                              "unexpected end of input"]}

    (p (p/+sep-by (tok :A) (tok :S))
       [:B])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :B"]}

    (p (p/+sep-by (tok :A) (tok :S))
       [:S])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :S"]}

    ))

(deftest *sep-by-t
  (test/are [expr result] (= result expr)

    (p (p/*sep-by (tok :A) (tok :S))
       [:A :S :A :S :A])
    {:consumed true, :value '(:A :A :A)}

    (p (p/*sep-by (tok :A) (tok :S))
       [:A :S :A :S :A :S])
    {:consumed true, :error ["error at index 6:"
                             "unexpected end of input"]}

    (p (p/*sep-by (tok :A) (tok :S))
       [:A :S :A :S :A :B])
    {:consumed true, :value '(:A :A :A)}

    (p (p/*sep-by (tok :A) (tok :S))
       [])
    {:consumed false, :value nil}

    (p (p/*sep-by (tok :A) (tok :S))
       [:B])
    {:consumed false, :value nil}

    (p (p/*sep-by (tok :A) (tok :S))
       [:S])
    {:consumed false, :value nil}

    ))

(deftest +sep-end-by-t
  (test/are [expr result] (= result expr)

    (p (p/+sep-end-by (tok :A) (tok :S))
       [:A :S :A :S :A :S])
    {:consumed true, :value '(:A :A :A)}

    (p (p/+sep-end-by (tok :A) (tok :S))
       [:A :S :A :S :A :S :A])
    {:consumed true, :error ["error at index 7:"
                             "unexpected end of input"]}

    (p (p/+sep-end-by (tok :A) (tok :S))
       [:A :S :A :S :A :S :B])
    {:consumed true, :value '(:A :A :A)}

    (p (p/+sep-end-by (tok :A) (tok :S))
       [:A :S :A :S :A])
    {:consumed true, :error ["error at index 5:"
                             "unexpected end of input"]}

    (p (p/+sep-end-by (tok :A) (tok :S))
       [:A :S :A :S :A :A])
    {:consumed true, :error ["error at index 5:"
                             "unexpected :A"]}

    (p (p/+sep-end-by (tok :A) (tok :S))
       [:A :S :A :S :A :B])
    {:consumed true, :error ["error at index 5:"
                             "unexpected :B"]}

    (p (p/+sep-end-by (tok :A) (tok :S))
       [])
    {:consumed false, :error ["error at index 0:"
                              "unexpected end of input"]}

    (p (p/+sep-end-by (tok :A) (tok :S))
       [:B])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :B"]}

    (p (p/+sep-end-by (tok :A) (tok :S))
       [:S])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :S"]}

    )
  )

(deftest *sep-end-by-t
  (test/are [expr result] (= result expr)

    (p (p/*sep-end-by (tok :A) (tok :S))
       [:A :S :A :S :A :S])
    {:consumed true, :value '(:A :A :A)}

    (p (p/*sep-end-by (tok :A) (tok :S))
       [:A :S :A :S :A :S :A])
    {:consumed true, :error ["error at index 7:"
                             "unexpected end of input"]}

    (p (p/*sep-end-by (tok :A) (tok :S))
       [:A :S :A :S :A :S :B])
    {:consumed true, :value '(:A :A :A)}

    (p (p/*sep-end-by (tok :A) (tok :S))
       [:A :S :A :S :A])
    {:consumed true, :error ["error at index 5:"
                             "unexpected end of input"]}

    (p (p/*sep-end-by (tok :A) (tok :S))
       [:A :S :A :S :A :A])
    {:consumed true, :error ["error at index 5:"
                             "unexpected :A"]}

    (p (p/*sep-end-by (tok :A) (tok :S))
       [:A :S :A :S :A :B])
    {:consumed true, :error ["error at index 5:"
                             "unexpected :B"]}

    (p (p/*sep-end-by (tok :A) (tok :S))
       [])
    {:consumed false, :value nil}

    (p (p/*sep-end-by (tok :A) (tok :S))
       [:B])
    {:consumed false, :value nil}

    (p (p/*sep-end-by (tok :A) (tok :S))
       [:S])
    {:consumed false, :value nil}

    ))

(deftest +sep-opt-by-t
  (test/are [expr result] (= result expr)

    (p (p/+sep-opt-by (tok :A) (tok :S))
       [:A :S :A :S :A :S])
    {:consumed true, :value '(:A :A :A)}

    (p (p/+sep-opt-by (tok :A) (tok :S))
       [:A :S :A :S :A :S :A])
    {:consumed true, :value '(:A :A :A :A)}

    (p (p/+sep-opt-by (tok :A) (tok :S))
       [:A :S :A :S :A :S :B])
    {:consumed true, :value '(:A :A :A)}

    (p (p/+sep-opt-by (tok :A) (tok :S))
       [:A :S :A :S :A])
    {:consumed true, :value '(:A :A :A)}

    (p (p/+sep-opt-by (tok :A) (tok :S))
       [:A :S :A :S :A :A])
    {:consumed true, :value '(:A :A :A)}

    (p (p/+sep-opt-by (tok :A) (tok :S))
       [:A :S :A :S :A :B])
    {:consumed true, :value '(:A :A :A)}

    (p (p/+sep-opt-by (tok :A) (tok :S))
       [])
    {:consumed false, :error ["error at index 0:"
                              "unexpected end of input"]}

    (p (p/+sep-opt-by (tok :A) (tok :S))
       [:B])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :B"]}

    (p (p/+sep-opt-by (tok :A) (tok :S))
       [:S])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :S"]}

    ))

(deftest *sep-opt-by-t
  (test/are [expr result] (= result expr)

    (p (p/*sep-opt-by (tok :A) (tok :S))
       [:A :S :A :S :A :S])
    {:consumed true, :value '(:A :A :A)}

    (p (p/*sep-opt-by (tok :A) (tok :S))
       [:A :S :A :S :A :S :A])
    {:consumed true, :value '(:A :A :A :A)}

    (p (p/*sep-opt-by (tok :A) (tok :S))
       [:A :S :A :S :A :S :B])
    {:consumed true, :value '(:A :A :A)}

    (p (p/*sep-opt-by (tok :A) (tok :S))
       [:A :S :A :S :A])
    {:consumed true, :value '(:A :A :A)}

    (p (p/*sep-opt-by (tok :A) (tok :S))
       [:A :S :A :S :A :A])
    {:consumed true, :value '(:A :A :A)}

    (p (p/*sep-opt-by (tok :A) (tok :S))
       [:A :S :A :S :A :B])
    {:consumed true, :value '(:A :A :A)}

    (p (p/*sep-opt-by (tok :A) (tok :S))
       [])
    {:consumed false, :value nil}

    (p (p/*sep-opt-by (tok :A) (tok :S))
       [:B])
    {:consumed false, :value nil}

    (p (p/*sep-opt-by (tok :A) (tok :S))
       [:S])
    {:consumed false, :value nil}

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest get-state-t
  (test/are [expr result] (= result expr)

    (-> (p/parse* (p/get-state) [:A])
        ((juxt (comp :input :value) (comp :input :state))))
    ['(:A) '(:A)]

    (-> (p/parse* (p/get-state :input) [:A])
        :value)
    '(:A)

    (-> (p/parse* (p/after (p/set-state :user ::state) (p/get-state :user)) [:A])
        :value)
    ::state

    ))

(deftest set-state-t
  (test/are [expr result] (= result expr)

    (-> (p/parse* (p/set-state ::state) [:A])
        :state)
    ::state

    (-> (p/parse* (p/set-state :input [:B]) [:A])
        :state :input)
    '(:B)

    (-> (p/parse* (p/set-state :input nil) [:A])
        :state :input)
    '()

    (-> (p/parse* (p/set-state :user ::state) [:A])
        :state :user)
    ::state

    ))

(deftest update-state-t
  (test/are [expr result] (= result expr)

    (-> (p/parse* (p/update-state (constantly ::state)) [:A])
        :state)
    ::state

    (-> (p/parse* (p/update-state :input (constantly [:B])) [:A])
        :state :input)
    '(:B)

    (-> (p/parse* (p/update-state :input (constantly nil)) [:A])
        :state :input)
    '()

    (-> (p/parse* (p/update-state :user (constantly ::state)) [:A])
        :state :user)
    ::state

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest trace-t
  (testing "trace state"
    (test/are [expr result] (= result expr)

      (-> (p (p/for [_ (p/trace "a")
                     a (tok :A)
                     _ (p/trace "b")
                     b (tok :B)]
               (p/result [a b]))
             [:A :B :C])
          (with-out-str)
          (string/split-lines))
      ["a: at index 0"
       " - input: (:A :B :C)"
       " - user: nil"
       "b: at index 1"
       " - input: (:B :C)"
       " - user: nil"]

      (-> (p (p/for [_ (p/trace "a")
                     a (tok :A)
                     _ (p/trace "b")
                     b (tok :B)]
               (p/result [a b]))
             [:A :B])
          (with-out-str)
          (string/split-lines))
      ["a: at index 0"
       " - input: (:A :B)"
       " - user: nil"
       "b: at index 1"
       " - input: (:B)"
       " - user: nil"]

      (-> (p (p/for [a (tok :A)
                     _ (p/trace "a")
                     b (tok :B)
                     _ (p/trace "b")]
               (p/result [a b]))
             [:A :B])
          (with-out-str)
          (string/split-lines))
      ["a: at index 1"
       " - input: (:B)"
       " - user: nil"
       "b: at index 2"
       " - input: ()"
       " - user: nil"]

      ))

  (testing "trace parser"
    (test/are [expr result] (= result expr)

      (-> (p (p/for [a (p/trace "a" (tok :A))
                     b (p/trace "b" (tok :B))]
               (p/result [a b]))
             [:A :B :C])
          (with-out-str)
          (string/split-lines))
      ["a: at index 0"
       " - input: (:A :B :C)"
       " - user: nil"
       "b: at index 1"
       " - input: (:B :C)"
       " - user: nil"]

      (-> (p (p/for [a (p/trace "a" (tok :A))
                     b (p/trace "b" (tok :B))]
               (p/result [a b]))
             [:A :B])
          (with-out-str)
          (string/split-lines))
      ["a: at index 0"
       " - input: (:A :B)"
       " - user: nil"
       "b: at index 1"
       " - input: (:B)"
       " - user: nil"]

      (-> (p (p/for [a (p/trace "a" (tok :A))
                     b (p/trace "b" (tok :B))]
               (p/result [a b]))
             [:B :C])
          (with-out-str)
          (string/split-lines))
      ["a: at index 0"
       " - input: (:B :C)"
       " - user: nil"
       "a: backtracked"]

      (-> (p (p/for [a (p/trace "a" (tok :A))
                     b (p/trace "b" (tok :B))]
               (p/result [a b]))
             [:A :C])
          (with-out-str)
          (string/split-lines))
      ["a: at index 0"
       " - input: (:A :C)"
       " - user: nil"
       "b: at index 1"
       " - input: (:C)"
       " - user: nil"
       "b: backtracked"]

      )))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest parse-t
  (test/are [expr result] (= result expr)

    (p/parse (p/result :ok) [])
    :ok

    (try (p/parse (p/fail "Error") [])
         (catch #?@(:clj [Exception e] :default [:default e])
                (ex-message e)))
    "error at index 0:\nError"

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
