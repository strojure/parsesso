(ns strojure.parsesso.parser-t
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
  (p/choice (p/bind-let [x parser] (p/fail (str "Test failure after parsing " x)))
            parser))

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

(deftest fmap-t
  (test/are [expr result] (= result expr)

    (p (p/fmap name (tok :A))
       [:A])
    {:consumed true, :value "A"}

    (p (p/fmap name (tok :A))
       [:B])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :B"]}

    (p (p/fmap name (tok :A))
       [])
    {:consumed false, :error ["error at index 0:"
                              "unexpected end of input"]}

    (p (p/fmap name (fail-consumed (tok :A)))
       [:A])
    {:consumed true, :error ["error at index 1:"
                             "Test failure after parsing :A"]}

    (p (p/fmap name (fail-consumed (tok :A)))
       [:B])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :B"]}

    (p (p/fmap name (fail-consumed (tok :A)))
       [])
    {:consumed false, :error ["error at index 0:"
                              "unexpected end of input"]}

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

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

(deftest offer-t
  (test/are [expr result] (= result expr)

    (p (p/offer (tok :A))
       [:A])
    {:consumed true, :value :A}

    (p (p/offer (tok :A))
       [:B])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :B"]}

    (p (p/offer (tok :A))
       [])
    {:consumed false, :error ["error at index 0:"
                              "unexpected end of input"]}

    (p (p/offer (fail-consumed (tok :A)))
       [:A])
    {:consumed false, :error ["error at index 1:"
                              "Test failure after parsing :A"]}

    (p (p/offer (fail-consumed (tok :A)))
       [:B])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :B"]}

    (p (p/offer (fail-consumed (tok :A)))
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
  (test/are [expr result] (= result expr)

    (p (p/not-followed-by (p/result :X)
                          (tok :A))
       [:B])
    {:consumed false, :value :X}

    (p (p/not-followed-by (p/result :X)
                          (p/each [(tok :A) (tok :B)]))
       [:A :A])
    {:consumed false, :value :X}

    (p (p/not-followed-by (p/result :X)
                          (tok :A))
       [])
    {:consumed false, :value :X}

    (p (p/not-followed-by (p/result :X)
                          p/any-token)
       [])
    {:consumed false, :value :X}

    (p (p/not-followed-by (p/result :X)
                          (tok :A))
       [:A])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :A"]}

    (p (p/not-followed-by (p/result :X)
                          (p/each [(tok :A) (tok :B)]))
       [:A :B])
    {:consumed false, :error ["error at index 0:"
                              "unexpected (:A :B)"]}

    (p (p/not-followed-by (p/result :X)
                          p/eof)
       [])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :strojure.parsesso.parser/eof"]}

    (p (p/not-followed-by (tok :X)
                          (tok :A))
       [:X :B])
    {:consumed true, :value :X}

    (p (p/not-followed-by (tok :X)
                          (p/each [(tok :A) (tok :B)]))
       [:X :A :A])
    {:consumed true, :value :X}

    (p (p/not-followed-by (tok :X)
                          (tok :A))
       [])
    {:consumed false, :error ["error at index 0:"
                              "unexpected end of input"]}

    (p (p/not-followed-by (tok :X)
                          (tok :A))
       [:X :A])
    {:consumed true, :error ["error at index 1:"
                             "unexpected :A"]}

    (p (p/not-followed-by (tok :X)
                          (p/each [(tok :A) (tok :B)]))
       [:X :A :B])
    {:consumed true, :error ["error at index 1:"
                             "unexpected (:A :B)"]}

    (p (p/not-followed-by (tok :X)
                          p/eof)
       [:X])
    {:consumed true, :error ["error at index 1:"
                             "unexpected :strojure.parsesso.parser/eof"]}

    ))

(deftest many-zero-t
  (test/are [expr result] (= result expr)

    (p (p/many-zero (tok :A :B :C))
       [:A :B :C :D :E :F])
    {:consumed true, :value [:A :B :C]}

    (p (p/many-zero (fail-consumed (tok :A :B :C)))
       [:A :B :C :D :E :F])
    {:consumed true, :error ["error at index 1:"
                             "Test failure after parsing :A"]}

    (p (p/many-zero (tok :D :E :F))
       [:A :B :C :D :E :F])
    {:consumed false, :value nil}

    (p (p/many-zero (tok :A :B :C))
       [])
    {:consumed false, :value nil}

    (p (p/many-zero (tok :A))
       (repeat 10000 :A))
    {:consumed true, :value (repeat 10000 :A)}

    ))

(deftest many-some-t
  (test/are [expr result] (= result expr)

    (p (p/many-some (tok :A :B :C))
       [:A :B :C :D :E :F])
    {:consumed true, :value [:A :B :C]}

    (p (p/many-some (tok :D :E :F))
       [:A :B :C :D :E :F])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :A"]}

    (p (p/many-some (tok :A :B :C))
       [])
    {:consumed false, :error ["error at index 0:"
                              "unexpected end of input"]}

    (p (p/many-some (tok :A))
       (repeat 10000 :A))
    {:consumed true, :value (repeat 10000 :A)}

    ))

(deftest skip-zero-t
  (test/are [expr result] (= result expr)

    (p (p/skip-zero (tok :A))
       [:A :A :A :B :B :B])
    {:consumed true, :value nil}

    (p (p/skip-zero (fail-consumed (tok :A)))
       [:A :A :A :B :B :B])
    {:consumed true, :error ["error at index 1:"
                             "Test failure after parsing :A"]}

    (p (p/skip-zero (tok :A))
       [:B :B :B])
    {:consumed false, :value nil}

    (p (p/skip-zero (tok :A))
       [])
    {:consumed false, :value nil}

    )
  )

(deftest skip-some-t
  (test/are [expr result] (= result expr)

    (p (p/skip-some (tok :A))
       [:A :A :A :B :B :B])
    {:consumed true, :value nil}

    (p (p/skip-some (tok :A))
       [:B :B :B])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :B"]}

    (p (p/skip-some (tok :A))
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

(deftest word-t
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

    ))

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
    {:consumed false, :value ::p/eof}

    (p p/eof
       [:A])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :A"
                              "expecting end of input"]}

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest each-t
  (test/are [expr result] (= result expr)

    (p (p/each [(tok :A) (tok :B) (tok :C)])
       [:A :B :C])
    '{:consumed true, :value (:A :B :C)}

    (p (p/each [(tok :A) (tok :B) (tok :C)])
       [:B :C])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :B"]}

    (p (p/each [(fail-consumed (tok :A)) (tok :B) (tok :C)])
       [:A :B :C])
    {:consumed true, :error ["error at index 1:"
                             "Test failure after parsing :A"]}

    (p (p/each [])
       [:A :B :C])
    {:consumed false, :value nil}

    (p (p/each nil)
       [:A :B :C])
    {:consumed false, :value nil}

    ))

(deftest tuple-t
  (test/are [expr result] (= result expr)

    (p (p/tuple (tok :A) (tok :B) (tok :C))
       [:A :B :C])
    '{:consumed true, :value (:A :B :C)}

    (p (p/tuple (tok :A) (tok :B) (tok :C))
       [:B :C])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :B"]}

    (p (p/tuple (fail-consumed (tok :A)) (tok :B) (tok :C))
       [:A :B :C])
    {:consumed true, :error ["error at index 1:"
                             "Test failure after parsing :A"]}

    ))

(deftest choice-t
  (test/are [expr result] (= result expr)

    (p (p/choice (tok :A)
                 (tok :B))
       [:A])
    {:consumed true, :value :A}

    (p (p/choice (tok :A)
                 (tok :B))
       [:B])
    {:consumed true, :value :B}

    (p (p/choice (tok :A)
                 (tok :B))
       [:C])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :C"]}

    (p (p/choice (tok :A)
                 (tok :B))
       [])
    {:consumed false, :error ["error at index 0:"
                              "unexpected end of input"]}

    (p (p/choice (fail-consumed (tok :A))
                 (tok :B))
       [:A])
    {:consumed true, :error ["error at index 1:"
                             "Test failure after parsing :A"]}

    (p (p/choice (fail-consumed (tok :A))
                 (tok :B))
       [:B])
    {:consumed true, :value :B}

    (p (p/choice (fail-consumed (tok :A))
                 (tok :B))
       [:C])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :C"]}

    (p (p/choice (fail-consumed (tok :A))
                 (tok :B))
       [])
    {:consumed false, :error ["error at index 0:"
                              "unexpected end of input"]}

    (p (p/choice (tok :A)
                 (fail-consumed (tok :B)))
       [:A])
    {:consumed true, :value :A}

    (p (p/choice (tok :A)
                 (fail-consumed (tok :B)))
       [:B])
    {:consumed true, :error ["error at index 1:"
                             "Test failure after parsing :B"]}

    (p (p/choice (tok :A)
                 (fail-consumed (tok :B)))
       [:C])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :C"]}

    (p (p/choice (tok :A)
                 (fail-consumed (tok :B)))
       [])
    {:consumed false, :error ["error at index 0:"
                              "unexpected end of input"]}

    (p (p/choice (p/expecting (tok :A) :A)
                 (p/expecting (tok :B) :B))
       [:C])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :C"
                              "expecting :A or :B"]}

    ))

(deftest optional-t
  (testing "The `optional` without default."
    (test/are [expr result] (= result expr)

      (p (p/optional (tok :A))
         [:A])
      {:consumed true, :value :A}

      (p (p/optional (tok :A))
         [:B])
      {:consumed false, :value nil}

      (p (p/optional (tok :A))
         [])
      {:consumed false, :value nil}

      (p (p/optional (fail-consumed (tok :A)))
         [:A])
      {:consumed true, :error ["error at index 1:"
                               "Test failure after parsing :A"]}

      (p (p/optional (fail-consumed (tok :A)))
         [:B])
      {:consumed false, :value nil}

      (p (p/optional (fail-consumed (tok :A)))
         [])
      {:consumed false, :value nil}

      ))

  (testing "The `optional` with default value."
    (test/are [expr result] (= result expr)

      (p (p/optional (tok :A) :X)
         [:A])
      {:consumed true, :value :A}

      (p (p/optional (tok :A) :X)
         [:B])
      {:consumed false, :value :X}

      (p (p/optional (tok :A) :X)
         [])
      {:consumed false, :value :X}

      (p (p/optional (fail-consumed (tok :A)) :X)
         [:A])
      {:consumed true, :error ["error at index 1:"
                               "Test failure after parsing :A"]}

      (p (p/optional (fail-consumed (tok :A)) :X)
         [:B])
      {:consumed false, :value :X}

      (p (p/optional (fail-consumed (tok :A)) :X)
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

(deftest many-till-t
  (test/are [expr result] (= result expr)

    (p (p/many-till (tok :A1 :A2 :A3)
                    (tok :END))
       [:A1 :A2 :A3 :END])
    {:consumed true, :value '(:A1 :A2 :A3)}

    (p (p/many-till (tok :A1 :A2 :A3)
                    (tok :END))
       [:A1 :A2 :A3 :B :END])
    {:consumed true, :error ["error at index 3:"
                             "unexpected :B"]}

    (p (p/many-till (tok :A1 :A2 :A3)
                    (tok :END))
       [:B :END])
    {:consumed false, :error ["error at index 0:"
                              "unexpected :B"]}

    (p (p/many-till (tok :A1 :A2 :A3)
                    (tok :END))
       [:A1 :A2 :A3])
    {:consumed true, :error ["error at index 3:"
                             "unexpected end of input"]}

    (p (p/many-till (fail-consumed (tok :A1 :A2 :A3))
                    (tok :END))
       [:A1 :A2 :A3 :END])
    {:consumed true, :error ["error at index 1:"
                             "Test failure after parsing :A1"]}

    (p (p/many-till (tok :A1 :A2 :A3)
                    (tok :END))
       [:END])
    {:consumed true, :value nil}

    (p (p/many-till (p/choice (tok :A1 :A2 :A3)
                              (p/many-till (tok :B1 :B2 :B3)
                                           (tok :END)))
                    (tok :END))
       [:A1 :A2 :A3 :B1 :B2 :B3 :END :A1 :A2 :A3 :END])
    {:consumed true, :value '(:A1 :A2 :A3 (:B1 :B2 :B3) :A1 :A2 :A3)}

    (p (p/many-till (tok :A1 :A2 :A3)
                    (tok :END))
       (concat (take 10000 (cycle [:A1 :A2 :A3])) [:END]))
    {:consumed true, :value (take 10000 (cycle [:A1 :A2 :A3]))}

    ))

(deftest sep-by-t

  (testing 'p/sep-by-some
    (test/are [expr result] (= result expr)

      (p (p/sep-by-some (tok :A) (tok :S))
         [:A :S :A :S :A])
      {:consumed true, :value '(:A :A :A)}

      (p (p/sep-by-some (tok :A) (tok :S))
         [:A :S :A :S :A :S])
      {:consumed true, :error ["error at index 6:"
                               "unexpected end of input"]}

      (p (p/sep-by-some (tok :A) (tok :S))
         [:A :S :A :S :A :B])
      {:consumed true, :value '(:A :A :A)}

      (p (p/sep-by-some (tok :A) (tok :S))
         [])
      {:consumed false, :error ["error at index 0:"
                                "unexpected end of input"]}

      (p (p/sep-by-some (tok :A) (tok :S))
         [:B])
      {:consumed false, :error ["error at index 0:"
                                "unexpected :B"]}

      (p (p/sep-by-some (tok :A) (tok :S))
         [:S])
      {:consumed false, :error ["error at index 0:"
                                "unexpected :S"]}

      ))

  (testing 'p/sep-by-zero
    (test/are [expr result] (= result expr)

      (p (p/sep-by-zero (tok :A) (tok :S))
         [:A :S :A :S :A])
      {:consumed true, :value '(:A :A :A)}

      (p (p/sep-by-zero (tok :A) (tok :S))
         [:A :S :A :S :A :S])
      {:consumed true, :error ["error at index 6:"
                               "unexpected end of input"]}

      (p (p/sep-by-zero (tok :A) (tok :S))
         [:A :S :A :S :A :B])
      {:consumed true, :value '(:A :A :A)}

      (p (p/sep-by-zero (tok :A) (tok :S))
         [])
      {:consumed false, :value nil}

      (p (p/sep-by-zero (tok :A) (tok :S))
         [:B])
      {:consumed false, :value nil}

      (p (p/sep-by-zero (tok :A) (tok :S))
         [:S])
      {:consumed false, :value nil}

      )))

(deftest sep-by-end-t

  (testing 'p/sep-by-end-some
    (test/are [expr result] (= result expr)

      (p (p/sep-by-end-some (tok :A) (tok :S))
         [:A :S :A :S :A :S])
      {:consumed true, :value '(:A :A :A)}

      (p (p/sep-by-end-some (tok :A) (tok :S))
         [:A :S :A :S :A :S :A])
      {:consumed true, :error ["error at index 7:"
                               "unexpected end of input"]}

      (p (p/sep-by-end-some (tok :A) (tok :S))
         [:A :S :A :S :A :S :B])
      {:consumed true, :value '(:A :A :A)}

      (p (p/sep-by-end-some (tok :A) (tok :S))
         [:A :S :A :S :A])
      {:consumed true, :error ["error at index 5:"
                               "unexpected end of input"]}

      (p (p/sep-by-end-some (tok :A) (tok :S))
         [:A :S :A :S :A :A])
      {:consumed true, :error ["error at index 5:"
                               "unexpected :A"]}

      (p (p/sep-by-end-some (tok :A) (tok :S))
         [:A :S :A :S :A :B])
      {:consumed true, :error ["error at index 5:"
                               "unexpected :B"]}

      (p (p/sep-by-end-some (tok :A) (tok :S))
         [])
      {:consumed false, :error ["error at index 0:"
                                "unexpected end of input"]}

      (p (p/sep-by-end-some (tok :A) (tok :S))
         [:B])
      {:consumed false, :error ["error at index 0:"
                                "unexpected :B"]}

      (p (p/sep-by-end-some (tok :A) (tok :S))
         [:S])
      {:consumed false, :error ["error at index 0:"
                                "unexpected :S"]}

      ))

  (testing 'p/sep-by-end-zero
    (test/are [expr result] (= result expr)

      (p (p/sep-by-end-zero (tok :A) (tok :S))
         [:A :S :A :S :A :S])
      {:consumed true, :value '(:A :A :A)}

      (p (p/sep-by-end-zero (tok :A) (tok :S))
         [:A :S :A :S :A :S :A])
      {:consumed true, :error ["error at index 7:"
                               "unexpected end of input"]}

      (p (p/sep-by-end-zero (tok :A) (tok :S))
         [:A :S :A :S :A :S :B])
      {:consumed true, :value '(:A :A :A)}

      (p (p/sep-by-end-zero (tok :A) (tok :S))
         [:A :S :A :S :A])
      {:consumed true, :error ["error at index 5:"
                               "unexpected end of input"]}

      (p (p/sep-by-end-zero (tok :A) (tok :S))
         [:A :S :A :S :A :A])
      {:consumed true, :error ["error at index 5:"
                               "unexpected :A"]}

      (p (p/sep-by-end-zero (tok :A) (tok :S))
         [:A :S :A :S :A :B])
      {:consumed true, :error ["error at index 5:"
                               "unexpected :B"]}

      (p (p/sep-by-end-zero (tok :A) (tok :S))
         [])
      {:consumed false, :value nil}

      (p (p/sep-by-end-zero (tok :A) (tok :S))
         [:B])
      {:consumed false, :value nil}

      (p (p/sep-by-end-zero (tok :A) (tok :S))
         [:S])
      {:consumed false, :value nil}

      )))

(deftest sep-by-opt-end-t

  (testing 'p/sep-by-opt-end-some
    (test/are [expr result] (= result expr)

      (p (p/sep-by-opt-end-some (tok :A) (tok :S))
         [:A :S :A :S :A :S])
      {:consumed true, :value '(:A :A :A)}

      (p (p/sep-by-opt-end-some (tok :A) (tok :S))
         [:A :S :A :S :A :S :A])
      {:consumed true, :value '(:A :A :A :A)}

      (p (p/sep-by-opt-end-some (tok :A) (tok :S))
         [:A :S :A :S :A :S :B])
      {:consumed true, :value '(:A :A :A)}

      (p (p/sep-by-opt-end-some (tok :A) (tok :S))
         [:A :S :A :S :A])
      {:consumed true, :value '(:A :A :A)}

      (p (p/sep-by-opt-end-some (tok :A) (tok :S))
         [:A :S :A :S :A :A])
      {:consumed true, :value '(:A :A :A)}

      (p (p/sep-by-opt-end-some (tok :A) (tok :S))
         [:A :S :A :S :A :B])
      {:consumed true, :value '(:A :A :A)}

      (p (p/sep-by-opt-end-some (tok :A) (tok :S))
         [])
      {:consumed false, :error ["error at index 0:"
                                "unexpected end of input"]}

      (p (p/sep-by-opt-end-some (tok :A) (tok :S))
         [:B])
      {:consumed false, :error ["error at index 0:"
                                "unexpected :B"]}

      (p (p/sep-by-opt-end-some (tok :A) (tok :S))
         [:S])
      {:consumed false, :error ["error at index 0:"
                                "unexpected :S"]}

      ))

  (testing 'p/sep-by-opt-end-zero
    (test/are [expr result] (= result expr)

      (p (p/sep-by-opt-end-zero (tok :A) (tok :S))
         [:A :S :A :S :A :S])
      {:consumed true, :value '(:A :A :A)}

      (p (p/sep-by-opt-end-zero (tok :A) (tok :S))
         [:A :S :A :S :A :S :A])
      {:consumed true, :value '(:A :A :A :A)}

      (p (p/sep-by-opt-end-zero (tok :A) (tok :S))
         [:A :S :A :S :A :S :B])
      {:consumed true, :value '(:A :A :A)}

      (p (p/sep-by-opt-end-zero (tok :A) (tok :S))
         [:A :S :A :S :A])
      {:consumed true, :value '(:A :A :A)}

      (p (p/sep-by-opt-end-zero (tok :A) (tok :S))
         [:A :S :A :S :A :A])
      {:consumed true, :value '(:A :A :A)}

      (p (p/sep-by-opt-end-zero (tok :A) (tok :S))
         [:A :S :A :S :A :B])
      {:consumed true, :value '(:A :A :A)}

      (p (p/sep-by-opt-end-zero (tok :A) (tok :S))
         [])
      {:consumed false, :value nil}

      (p (p/sep-by-opt-end-zero (tok :A) (tok :S))
         [:B])
      {:consumed false, :value nil}

      (p (p/sep-by-opt-end-zero (tok :A) (tok :S))
         [:S])
      {:consumed false, :value nil}

      )))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest debug-state-t
  (test/are [expr result] (= result expr)

    (-> (p (p/bind-let [_ (p/debug-state "a") a (tok :A)
                        _ (p/debug-state "b") b (tok :B)]
             (p/result [a b]))
           [:A :B :C])
        (with-out-str)
        (string/split-lines))
    ["a: (:A :B :C)"
     "b: (:B :C)"]

    (-> (p (p/bind-let [_ (p/debug-state "a") a (tok :A)
                        _ (p/debug-state "b") b (tok :B)]
             (p/result [a b]))
           [:A :B])
        (with-out-str)
        (string/split-lines))
    ["a: (:A :B)"
     "b: (:B)"]

    (-> (p (p/bind-let [a (tok :A) _ (p/debug-state "a")
                        b (tok :B) _ (p/debug-state "b")]
             (p/result [a b]))
           [:A :B])
        (with-out-str)
        (string/split-lines))
    ["a: (:B)"]

    ))

(deftest debug-parser-t
  (test/are [expr result] (= result expr)

    (-> (p (p/bind-let [a (p/debug-parser (tok :A) "a")
                        b (p/debug-parser (tok :B) "b")]
             (p/result [a b]))
           [:A :B :C])
        (with-out-str)
        (string/split-lines))
    ["a: (:A :B :C)"
     "b: (:B :C)"]

    (-> (p (p/bind-let [a (p/debug-parser (tok :A) "a")
                        b (p/debug-parser (tok :B) "b")]
             (p/result [a b]))
           [:A :B])
        (with-out-str)
        (string/split-lines))
    ["a: (:A :B)"
     "b: (:B)"]

    (-> (p (p/bind-let [a (p/debug-parser (tok :A) "a")
                        b (p/debug-parser (tok :B) "b")]
             (p/result [a b]))
           [:B :C])
        (with-out-str)
        (string/split-lines))
    ["a: (:B :C)"
     "a backtracked"]

    (-> (p (p/bind-let [a (p/debug-parser (tok :A) "a")
                        b (p/debug-parser (tok :B) "b")]
             (p/result [a b]))
           [:A :C])
        (with-out-str)
        (string/split-lines))
    ["a: (:A :C)"
     "b: (:C)"
     "b backtracked"]

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
