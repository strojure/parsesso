(ns strojure.parsesso.core-test
  (:require [clojure.string :as string]
            [clojure.test :as test :refer [deftest testing]]
            [strojure.parsesso.core :as p]))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

#_(test/run-tests)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn- p
  "Parses test input using given parser. Returns custom map with test result."
  [parser input]
  (let [result (p/parse parser input)]
    (if (p/error? result)
      (-> (select-keys result [:consumed])
          (assoc :error (-> (:error result) (str) (string/split-lines))))
      (select-keys result [:consumed :value]))))

(defn- tok
  [& cs]
  (p/token (set cs)))

(defn- fail-consumed
  "Returns parser which fails when `p` is successfully consumed."
  [parser]
  (p/choice (p/when-let [x parser] (p/fail (str "Test failure after parsing " x)))
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

    (p (p/fail "Oops")
       [])
    {:consumed false, :error ["at index 0:"
                              "Oops"]}
    (p (p/fail "Oops")
       [:A])
    {:consumed false, :error ["at index 0:"
                              "Oops"]}
    ))

(deftest expecting-t
  (test/are [expr result] (= result expr)

    (p (-> (p/fail "Fail")
           (p/expecting "description"))
       [])
    {:consumed false, :error ["at index 0:"
                              "expecting description"
                              "Fail"]}

    (p (-> (p/fail "Fail")
           (p/expecting (delay "description")))
       [])
    {:consumed false, :error ["at index 0:"
                              "expecting description"
                              "Fail"]}

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest token-t
  (test/are [expr result] (= result expr)

    (p (p/token #{:A})
       [:A])
    {:consumed true, :value :A}

    (p (p/token #{:A})
       [:B])
    {:consumed false, :error ["at index 0:"
                              "unexpected :B"]}

    (p (p/token #{:A})
       [])
    {:consumed false, :error ["at index 0:"
                              "unexpected end of input"]}

    (p (fail-consumed (p/token #{:A}))
       [:A])
    {:consumed true, :error ["at index 1:"
                             "Test failure after parsing :A"]}

    (p (fail-consumed (p/token #{:A}))
       [:B])
    {:consumed false, :error ["at index 0:"
                              "unexpected :B"]}

    (p (fail-consumed (p/token #{:A}))
       [])
    {:consumed false, :error ["at index 0:"
                              "unexpected end of input"]}

    ))

(deftest any-token-t
  (test/are [expr result] (= result expr)

    (p p/any-token
       [:A])
    {:consumed true, :value :A}

    (p p/any-token
       [])
    {:consumed false, :error ["at index 0:"
                              "unexpected end of input"]}

    ))

(deftest not-followed-by-t
  (test/are [expr result] (= result expr)

    (p (p/not-followed-by (tok :A))
       [:A])
    {:consumed false, :error ["at index 0:"
                              "unexpected :A"]}

    (p (p/not-followed-by (p/sequence [(tok :A) (tok :B)]))
       [:A :B])
    {:consumed false, :error ["at index 0:"
                              "unexpected (:A :B)"]}

    (p (p/not-followed-by p/eof)
       [])
    {:consumed false, :error ["at index 0:"
                              "unexpected nil"]}

    (p (p/not-followed-by (tok :A))
       [:B])
    {:consumed false, :value nil}

    (p (p/not-followed-by (p/sequence [(tok :A) (tok :B)]))
       [:A :A])
    {:consumed false, :value nil}

    (p (p/not-followed-by (tok :A))
       [])
    {:consumed false, :value nil}

    (p (p/not-followed-by p/any-token)
       [])
    {:consumed false, :value nil}

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest bind-t
  (test/are [expr result] (= result expr)

    (p (p/bind (tok :A) p/result)
       [:A])
    {:consumed true, :value :A}

    (p (p/bind (tok :A) (fn [_] (p/fail "Oops")))
       [:A])
    {:consumed true, :error ["at index 1:"
                             "Oops"]}

    (p (p/bind (tok :A) p/result)
       [:B])
    {:consumed false, :error ["at index 0:"
                              "unexpected :B"]}

    (p (p/bind (tok :A) (fn [_] (p/fail "Oops")))
       [:B])
    {:consumed false, :error ["at index 0:"
                              "unexpected :B"]}

    (p (p/bind (tok :A) (fn [_] (tok :B)))
       [:A :B])
    {:consumed true, :value :B}

    (p (p/bind (tok :A) (fn [_] (tok :B)))
       [:B :A])
    {:consumed false, :error ["at index 0:"
                              "unexpected :B"]}

    (p (p/bind (tok :A) (fn [_] (tok :B)))
       [:A :A])
    {:consumed true, :error ["at index 1:"
                             "unexpected :A"]}

    ))

(deftest after-t
  (test/are [expr result] (= result expr)

    (p (p/after (tok :A) (tok :B))
       [:A :B])
    {:consumed true, :value :B}

    (p (p/after (tok :A) (tok :B))
       [:A :A])
    {:consumed true, :error ["at index 1:"
                             "unexpected :A"]}

    (p (p/after (tok :A) (tok :B))
       [:A])
    {:consumed true, :error ["at index 1:"
                             "unexpected end of input"]}

    (p (p/after (fail-consumed (tok :A)) (tok :B))
       [:A :B])
    {:consumed true, :error ["at index 1:"
                             "Test failure after parsing :A"]}

    (p (p/after (tok :A) (fail-consumed (tok :B)))
       [:A :B])
    {:consumed true, :error ["at index 2:"
                             "Test failure after parsing :B"]}

    (p (p/after (tok :A) (tok :B) (tok :C))
       [:A :B :C])
    {:consumed true, :value :C}

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
    {:consumed false, :error ["at index 0:"
                              "unexpected :C"]}

    (p (p/choice (tok :A)
                 (tok :B))
       [])
    {:consumed false, :error ["at index 0:"
                              "unexpected end of input"]}

    (p (p/choice (fail-consumed (tok :A))
                 (tok :B))
       [:A])
    {:consumed true, :error ["at index 1:"
                             "Test failure after parsing :A"]}

    (p (p/choice (fail-consumed (tok :A))
                 (tok :B))
       [:B])
    {:consumed true, :value :B}

    (p (p/choice (fail-consumed (tok :A))
                 (tok :B))
       [:C])
    {:consumed false, :error ["at index 0:"
                              "unexpected :C"]}

    (p (p/choice (fail-consumed (tok :A))
                 (tok :B))
       [])
    {:consumed false, :error ["at index 0:"
                              "unexpected end of input"]}

    (p (p/choice (tok :A)
                 (fail-consumed (tok :B)))
       [:A])
    {:consumed true, :value :A}

    (p (p/choice (tok :A)
                 (fail-consumed (tok :B)))
       [:B])
    {:consumed true, :error ["at index 1:"
                             "Test failure after parsing :B"]}

    (p (p/choice (tok :A)
                 (fail-consumed (tok :B)))
       [:C])
    {:consumed false, :error ["at index 0:"
                              "unexpected :C"]}

    (p (p/choice (tok :A)
                 (fail-consumed (tok :B)))
       [])
    {:consumed false, :error ["at index 0:"
                              "unexpected end of input"]}

    (p (p/choice (p/expecting (tok :A) :A)
                 (p/expecting (tok :B) :B))
       [:C])
    {:consumed false, :error ["at index 0:"
                              "unexpected :C"
                              "expecting :A or :B"]}

    ))

(deftest fmap-t
  (test/are [expr result] (= result expr)

    (p (p/fmap name (tok :A))
       [:A])
    {:consumed true, :value "A"}

    (p (p/fmap name (tok :A))
       [:B])
    {:consumed false, :error ["at index 0:"
                              "unexpected :B"]}

    (p (p/fmap name (tok :A))
       [])
    {:consumed false, :error ["at index 0:"
                              "unexpected end of input"]}

    (p (p/fmap name (fail-consumed (tok :A)))
       [:A])
    {:consumed true, :error ["at index 1:"
                             "Test failure after parsing :A"]}

    (p (p/fmap name (fail-consumed (tok :A)))
       [:B])
    {:consumed false, :error ["at index 0:"
                              "unexpected :B"]}

    (p (p/fmap name (fail-consumed (tok :A)))
       [])
    {:consumed false, :error ["at index 0:"
                              "unexpected end of input"]}

    ))

(deftest sequence-t
  (test/are [expr result] (= result expr)

    (p (p/sequence [(tok :A) (tok :B) (tok :C)])
       [:A :B :C])
    '{:consumed true, :value (:A :B :C)}

    (p (p/sequence [(tok :A) (tok :B) (tok :C)])
       [:B :C])
    {:consumed false, :error ["at index 0:"
                              "unexpected :B"]}

    (p (p/sequence [(fail-consumed (tok :A)) (tok :B) (tok :C)])
       [:A :B :C])
    {:consumed true, :error ["at index 1:"
                             "Test failure after parsing :A"]}

    (p (p/sequence [])
       [:A :B :C])
    {:consumed false, :value nil}

    (p (p/sequence nil)
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
    {:consumed false, :error ["at index 0:"
                              "unexpected :B"]}

    (p (p/tuple (fail-consumed (tok :A)) (tok :B) (tok :C))
       [:A :B :C])
    {:consumed true, :error ["at index 1:"
                             "Test failure after parsing :A"]}

    ))

(deftest silent-t
  (test/are [expr result] (= result expr)

    (p (p/silent (tok :A))
       [:A])
    {:consumed true, :value :A}

    (p (p/silent (tok :A))
       [:B])
    {:consumed false, :error ["at index 0:"
                              "unexpected :B"]}

    (p (p/silent (tok :A))
       [])
    {:consumed false, :error ["at index 0:"
                              "unexpected end of input"]}

    (p (p/silent (fail-consumed (tok :A)))
       [:A])
    {:consumed false, :error ["at index 1:"
                              "Test failure after parsing :A"]}

    (p (p/silent (fail-consumed (tok :A)))
       [:B])
    {:consumed false, :error ["at index 0:"
                              "unexpected :B"]}

    (p (p/silent (fail-consumed (tok :A)))
       [])
    {:consumed false, :error ["at index 0:"
                              "unexpected end of input"]}

    ))

(deftest look-ahead-t
  (test/are [expr result] (= result expr)

    (p (p/look-ahead (tok :A))
       [:A])
    {:consumed false, :value :A}

    (p (p/look-ahead (tok :A))
       [:B])
    {:consumed false, :error ["at index 0:"
                              "unexpected :B"]}

    (p (p/look-ahead (tok :A))
       [])
    {:consumed false, :error ["at index 0:"
                              "unexpected end of input"]}

    (p (p/look-ahead (fail-consumed (tok :A)))
       [:A])
    {:consumed true, :error ["at index 1:"
                             "Test failure after parsing :A"]}

    (p (p/look-ahead (fail-consumed (tok :A)))
       [:B])
    {:consumed false, :error ["at index 0:"
                              "unexpected :B"]}

    (p (p/look-ahead (fail-consumed (tok :A)))
       [])
    {:consumed false, :error ["at index 0:"
                              "unexpected end of input"]}

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
      {:consumed true, :error ["at index 1:"
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
      {:consumed true, :error ["at index 1:"
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
    {:consumed false, :error ["at index 0:"
                              "unexpected :R"]}

    (p (p/between (tok :A) (tok :L) (tok :R))
       [:L :A])
    {:consumed true, :error ["at index 2:"
                             "unexpected end of input"]}

    (p (p/between (tok :A) (tok :L) (tok :R))
       [:A :R])
    {:consumed false, :error ["at index 0:"
                              "unexpected :A"]}

    (p (p/between (tok :A) (tok :L) (tok :R))
       [:A])
    {:consumed false, :error ["at index 0:"
                              "unexpected :A"]}

    (p (p/between (tok :A) (tok :L) (tok :R))
       [])
    {:consumed false, :error ["at index 0:"
                              "unexpected end of input"]}

    (p (p/between (tok :A) (tok :I))
       [:I :A :I])
    {:consumed true, :value :A}

    (p (p/between (tok :A) (tok :I))
       [:I :A])
    {:consumed true, :error ["at index 2:"
                             "unexpected end of input"]}

    (p (p/between (tok :A) (tok :I))
       [:A :I])
    {:consumed false, :error ["at index 0:"
                              "unexpected :A"]}

    (p (p/between (tok :A) (tok :I))
       [:A])
    {:consumed false, :error ["at index 0:"
                              "unexpected :A"]}

    (p (p/between (tok :A) (tok :I))
       [])
    {:consumed false, :error ["at index 0:"
                              "unexpected end of input"]}

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest many-t

  (testing 'p/many
    (test/are [expr result] (= result expr)

      (p (p/many (tok :A :B :C))
         [:A :B :C :D :E :F])
      {:consumed true, :value [:A :B :C]}

      (p (p/many (tok :D :E :F))
         [:A :B :C :D :E :F])
      {:consumed false, :value nil}

      (p (p/many (tok :A :B :C))
         [])
      {:consumed false, :value nil}

      (p (p/many (tok :A))
         (repeat 10000 :A))
      {:consumed true, :value (repeat 10000 :A)}

      ))

  (testing 'p/some-many
    (test/are [expr result] (= result expr)

      (p (p/some-many (tok :A :B :C))
         [:A :B :C :D :E :F])
      {:consumed true, :value [:A :B :C]}

      (p (p/some-many (tok :D :E :F))
         [:A :B :C :D :E :F])
      {:consumed false, :error ["at index 0:"
                                "unexpected :A"]}

      (p (p/some-many (tok :A :B :C))
         [])
      {:consumed false, :error ["at index 0:"
                                "unexpected end of input"]}

      (p (p/some-many (tok :A))
         (repeat 10000 :A))
      {:consumed true, :value (repeat 10000 :A)}

      )))

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
    {:consumed true, :error ["at index 2:"
                             "unexpected end of input"]}

    (p (p/times 3 (tok :A1 :A2 :A3))
       [:A1 :A2 :B])
    {:consumed true, :error ["at index 2:"
                             "unexpected :B"]}

    (p (p/times 3 (tok :A1 :A2 :A3))
       [:B :A1 :A2 :A3])
    {:consumed false, :error ["at index 0:"
                              "unexpected :B"]}

    (p (p/times 3 (tok :A1 :A2 :A3))
       [:B :A1])
    {:consumed false, :error ["at index 0:"
                              "unexpected :B"]}

    (p (p/times 3 (tok :A1 :A2 :A3))
       [])
    {:consumed false, :error ["at index 0:"
                              "unexpected end of input"]}

    (p (p/times 0 (tok :A1 :A2 :A3))
       [:A1 :A2 :A3])
    {:consumed false, :value nil}

    (p (p/times -3 (tok :A1 :A2 :A3))
       [:A1 :A2 :A3])
    {:consumed false, :value nil}

    ))

(deftest skip-many-t

  (testing 'p/skip-many
    (test/are [expr result] (= result expr)

      (p (p/skip-many (tok :A))
         [:A :A :A :B :B :B])
      {:consumed true, :value nil}

      (p (p/skip-many (tok :A))
         [:B :B :B])
      {:consumed false, :value nil}

      (p (p/skip-many (tok :A))
         [])
      {:consumed false, :value nil}

      ))

  (testing 'p/some-skip-many
    (test/are [expr result] (= result expr)

      (p (p/some-skip-many (tok :A))
         [:A :A :A :B :B :B])
      {:consumed true, :value nil}

      (p (p/some-skip-many (tok :A))
         [:B :B :B])
      {:consumed false, :error ["at index 0:"
                                "unexpected :B"]}

      (p (p/some-skip-many (tok :A))
         [])
      {:consumed false, :error ["at index 0:"
                                "unexpected end of input"]}

      )))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest sep-by-t

  (testing 'p/some-sep-by
    (test/are [expr result] (= result expr)

      (p (p/some-sep-by (tok :A) (tok :S))
         [:A :S :A :S :A])
      {:consumed true, :value '(:A :A :A)}

      (p (p/some-sep-by (tok :A) (tok :S))
         [:A :S :A :S :A :S])
      {:consumed true, :error ["at index 6:"
                               "unexpected end of input"]}

      (p (p/some-sep-by (tok :A) (tok :S))
         [:A :S :A :S :A :B])
      {:consumed true, :value '(:A :A :A)}

      (p (p/some-sep-by (tok :A) (tok :S))
         [])
      {:consumed false, :error ["at index 0:"
                                "unexpected end of input"]}

      (p (p/some-sep-by (tok :A) (tok :S))
         [:B])
      {:consumed false, :error ["at index 0:"
                                "unexpected :B"]}

      (p (p/some-sep-by (tok :A) (tok :S))
         [:S])
      {:consumed false, :error ["at index 0:"
                                "unexpected :S"]}

      ))

  (testing 'p/sep-by
    (test/are [expr result] (= result expr)

      (p (p/sep-by (tok :A) (tok :S))
         [:A :S :A :S :A])
      {:consumed true, :value '(:A :A :A)}

      (p (p/sep-by (tok :A) (tok :S))
         [:A :S :A :S :A :S])
      {:consumed true, :error ["at index 6:"
                               "unexpected end of input"]}

      (p (p/sep-by (tok :A) (tok :S))
         [:A :S :A :S :A :B])
      {:consumed true, :value '(:A :A :A)}

      (p (p/sep-by (tok :A) (tok :S))
         [])
      {:consumed false, :value nil}

      (p (p/sep-by (tok :A) (tok :S))
         [:B])
      {:consumed false, :value nil}

      (p (p/sep-by (tok :A) (tok :S))
         [:S])
      {:consumed false, :value nil}

      )))

(deftest sep-by-end-t

  (testing 'p/some-sep-by-end
    (test/are [expr result] (= result expr)

      (p (p/some-sep-by-end (tok :A) (tok :S))
         [:A :S :A :S :A :S])
      {:consumed true, :value '(:A :A :A)}

      (p (p/some-sep-by-end (tok :A) (tok :S))
         [:A :S :A :S :A :S :A])
      {:consumed true, :error ["at index 7:"
                               "unexpected end of input"]}

      (p (p/some-sep-by-end (tok :A) (tok :S))
         [:A :S :A :S :A :S :B])
      {:consumed true, :value '(:A :A :A)}

      (p (p/some-sep-by-end (tok :A) (tok :S))
         [:A :S :A :S :A])
      {:consumed true, :error ["at index 5:"
                               "unexpected end of input"]}

      (p (p/some-sep-by-end (tok :A) (tok :S))
         [:A :S :A :S :A :A])
      {:consumed true, :error ["at index 5:"
                               "unexpected :A"]}

      (p (p/some-sep-by-end (tok :A) (tok :S))
         [:A :S :A :S :A :B])
      {:consumed true, :error ["at index 5:"
                               "unexpected :B"]}

      (p (p/some-sep-by-end (tok :A) (tok :S))
         [])
      {:consumed false, :error ["at index 0:"
                                "unexpected end of input"]}

      (p (p/some-sep-by-end (tok :A) (tok :S))
         [:B])
      {:consumed false, :error ["at index 0:"
                                "unexpected :B"]}

      (p (p/some-sep-by-end (tok :A) (tok :S))
         [:S])
      {:consumed false, :error ["at index 0:"
                                "unexpected :S"]}

      ))

  (testing 'p/sep-by-end
    (test/are [expr result] (= result expr)

      (p (p/sep-by-end (tok :A) (tok :S))
         [:A :S :A :S :A :S])
      {:consumed true, :value '(:A :A :A)}

      (p (p/sep-by-end (tok :A) (tok :S))
         [:A :S :A :S :A :S :A])
      {:consumed true, :error ["at index 7:"
                               "unexpected end of input"]}

      (p (p/sep-by-end (tok :A) (tok :S))
         [:A :S :A :S :A :S :B])
      {:consumed true, :value '(:A :A :A)}

      (p (p/sep-by-end (tok :A) (tok :S))
         [:A :S :A :S :A])
      {:consumed true, :error ["at index 5:"
                               "unexpected end of input"]}

      (p (p/sep-by-end (tok :A) (tok :S))
         [:A :S :A :S :A :A])
      {:consumed true, :error ["at index 5:"
                               "unexpected :A"]}

      (p (p/sep-by-end (tok :A) (tok :S))
         [:A :S :A :S :A :B])
      {:consumed true, :error ["at index 5:"
                               "unexpected :B"]}

      (p (p/sep-by-end (tok :A) (tok :S))
         [])
      {:consumed false, :value nil}

      (p (p/sep-by-end (tok :A) (tok :S))
         [:B])
      {:consumed false, :value nil}

      (p (p/sep-by-end (tok :A) (tok :S))
         [:S])
      {:consumed false, :value nil}

      )))

(deftest sep-by-opt-end-t

  (testing 'p/some-sep-by-opt-end
    (test/are [expr result] (= result expr)

      (p (p/some-sep-by-opt-end (tok :A) (tok :S))
         [:A :S :A :S :A :S])
      {:consumed true, :value '(:A :A :A)}

      (p (p/some-sep-by-opt-end (tok :A) (tok :S))
         [:A :S :A :S :A :S :A])
      {:consumed true, :value '(:A :A :A :A)}

      (p (p/some-sep-by-opt-end (tok :A) (tok :S))
         [:A :S :A :S :A :S :B])
      {:consumed true, :value '(:A :A :A)}

      (p (p/some-sep-by-opt-end (tok :A) (tok :S))
         [:A :S :A :S :A])
      {:consumed true, :value '(:A :A :A)}

      (p (p/some-sep-by-opt-end (tok :A) (tok :S))
         [:A :S :A :S :A :A])
      {:consumed true, :value '(:A :A :A)}

      (p (p/some-sep-by-opt-end (tok :A) (tok :S))
         [:A :S :A :S :A :B])
      {:consumed true, :value '(:A :A :A)}

      (p (p/some-sep-by-opt-end (tok :A) (tok :S))
         [])
      {:consumed false, :error ["at index 0:"
                                "unexpected end of input"]}

      (p (p/some-sep-by-opt-end (tok :A) (tok :S))
         [:B])
      {:consumed false, :error ["at index 0:"
                                "unexpected :B"]}

      (p (p/some-sep-by-opt-end (tok :A) (tok :S))
         [:S])
      {:consumed false, :error ["at index 0:"
                                "unexpected :S"]}

      ))

  (testing 'p/sep-by-opt-end
    (test/are [expr result] (= result expr)

      (p (p/sep-by-opt-end (tok :A) (tok :S))
         [:A :S :A :S :A :S])
      {:consumed true, :value '(:A :A :A)}

      (p (p/sep-by-opt-end (tok :A) (tok :S))
         [:A :S :A :S :A :S :A])
      {:consumed true, :value '(:A :A :A :A)}

      (p (p/sep-by-opt-end (tok :A) (tok :S))
         [:A :S :A :S :A :S :B])
      {:consumed true, :value '(:A :A :A)}

      (p (p/sep-by-opt-end (tok :A) (tok :S))
         [:A :S :A :S :A])
      {:consumed true, :value '(:A :A :A)}

      (p (p/sep-by-opt-end (tok :A) (tok :S))
         [:A :S :A :S :A :A])
      {:consumed true, :value '(:A :A :A)}

      (p (p/sep-by-opt-end (tok :A) (tok :S))
         [:A :S :A :S :A :B])
      {:consumed true, :value '(:A :A :A)}

      (p (p/sep-by-opt-end (tok :A) (tok :S))
         [])
      {:consumed false, :value nil}

      (p (p/sep-by-opt-end (tok :A) (tok :S))
         [:B])
      {:consumed false, :value nil}

      (p (p/sep-by-opt-end (tok :A) (tok :S))
         [:S])
      {:consumed false, :value nil}

      )))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest chain-left-t
  (testing 'p/some-chain-left
    (test/are [expr result] (= result expr)

      (p (p/some-chain-left (tok 1 2 3 3 4 5 6 7 8 9)
                            (tok + - * /))
         [8 - 2 / 2])
      {:consumed true, :value 3}

      (p (p/some-chain-left (tok 1 2 3 3 4 5 6 7 8 9)
                            (tok + - * /))
         [8 - 2 2])
      {:consumed true, :value 6}

      (p (p/some-chain-left (tok 1 2 3 3 4 5 6 7 8 9)
                            (tok + - * /))
         [1])
      {:consumed true, :value 1}

      (p (p/some-chain-left (tok 1 2 3 3 4 5 6 7 8 9)
                            (tok + - * /))
         [+])
      {:consumed false, :error ["at index 0:"
                                (str "unexpected " (pr-str +))]}

      (p (p/some-chain-left (tok 1 2 3 3 4 5 6 7 8 9)
                            (tok + - * /))
         [0])
      {:consumed false, :error ["at index 0:"
                                "unexpected 0"]}

      (p (p/some-chain-left (tok 1 2 3 3 4 5 6 7 8 9)
                            (tok + - * /))
         [])
      {:consumed false, :error ["at index 0:"
                                "unexpected end of input"]}

      ))

  (testing 'p/chain-left
    (test/are [expr result] (= result expr)

      (p (p/chain-left (tok 1 2 3 3 4 5 6 7 8 9)
                       (tok + - * /)
                       0)
         [8 - 2 / 2])
      {:consumed true, :value 3}

      (p (p/chain-left (tok 1 2 3 3 4 5 6 7 8 9)
                       (tok + - * /)
                       0)
         [8 - 2 2])
      {:consumed true, :value 6}

      (p (p/chain-left (tok 1 2 3 3 4 5 6 7 8 9)
                       (tok + - * /)
                       0)
         [1])
      {:consumed true, :value 1}

      (p (p/chain-left (tok 1 2 3 3 4 5 6 7 8 9)
                       (tok + - * /)
                       0)
         [+])
      {:consumed false, :value 0}

      (p (p/chain-left (tok 1 2 3 3 4 5 6 7 8 9)
                       (tok + - * /)
                       0)
         [0])
      {:consumed false, :value 0}

      (p (p/chain-left (tok 1 2 3 3 4 5 6 7 8 9)
                       (tok + - * /)
                       0)
         [])
      {:consumed false, :value 0}

      )))

(deftest chain-right-t
  (testing 'p/some-chain-right
    (test/are [expr result] (= result expr)

      (p (p/some-chain-right (tok 1 2 3 3 4 5 6 7 8 9)
                             (tok + - * /))
         [8 - 2 / 2])
      {:consumed true, :value 7}

      (p (p/some-chain-right (tok 1 2 3 3 4 5 6 7 8 9)
                             (tok + - * /))
         [8 - 2 2])
      {:consumed true, :value 6}

      (p (p/some-chain-right (tok 1 2 3 3 4 5 6 7 8 9)
                             (tok + - * /))
         [1])
      {:consumed true, :value 1}

      (p (p/some-chain-right (tok 1 2 3 3 4 5 6 7 8 9)
                             (tok + - * /))
         [+])
      {:consumed false, :error ["at index 0:"
                                (str "unexpected " (pr-str +))]}

      (p (p/some-chain-right (tok 1 2 3 3 4 5 6 7 8 9)
                             (tok + - * /))
         [0])
      {:consumed false, :error ["at index 0:"
                                "unexpected 0"]}

      (p (p/some-chain-right (tok 1 2 3 3 4 5 6 7 8 9)
                             (tok + - * /))
         [])
      {:consumed false, :error ["at index 0:"
                                "unexpected end of input"]}

      ))

  (testing 'p/chain-right
    (test/are [expr result] (= result expr)

      (p (p/chain-right (tok 1 2 3 3 4 5 6 7 8 9)
                        (tok + - * /)
                        0)
         [8 - 2 / 2])
      {:consumed true, :value 7}

      (p (p/chain-right (tok 1 2 3 3 4 5 6 7 8 9)
                        (tok + - * /)
                        0)
         [8 - 2 2])
      {:consumed true, :value 6}

      (p (p/chain-right (tok 1 2 3 3 4 5 6 7 8 9)
                        (tok + - * /)
                        0)
         [1])
      {:consumed true, :value 1}

      (p (p/chain-right (tok 1 2 3 3 4 5 6 7 8 9)
                        (tok + - * /)
                        0)
         [+])
      {:consumed false, :value 0}

      (p (p/chain-right (tok 1 2 3 3 4 5 6 7 8 9)
                        (tok + - * /)
                        0)
         [0])
      {:consumed false, :value 0}

      (p (p/chain-right (tok 1 2 3 3 4 5 6 7 8 9)
                        (tok + - * /)
                        0)
         [])
      {:consumed false, :value 0}

      )))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest eof-t
  (test/are [expr result] (= result expr)

    (p p/eof
       [])
    {:consumed false, :value nil}

    (p p/eof
       [:A])
    {:consumed false, :error ["at index 0:"
                              "unexpected :A"
                              "expecting end of input"]}

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
    {:consumed true, :error ["at index 3:"
                             "unexpected :B"]}

    (p (p/many-till (tok :A1 :A2 :A3)
                    (tok :END))
       [:B :END])
    {:consumed false, :error ["at index 0:"
                              "unexpected :B"]}

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

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest debug-state-t
  (test/are [expr result] (= result expr)

    (-> (p (p/when-let [_ (p/debug-state "a") a (tok :A)
                        _ (p/debug-state "b") b (tok :B)]
             (p/result [a b]))
           [:A :B :C])
        (with-out-str)
        (string/split-lines))
    ["a: (:A :B :C)"
     "b: (:B :C)"]

    (-> (p (p/when-let [_ (p/debug-state "a") a (tok :A)
                        _ (p/debug-state "b") b (tok :B)]
             (p/result [a b]))
           [:A :B])
        (with-out-str)
        (string/split-lines))
    ["a: (:A :B)"
     "b: (:B)"]

    (-> (p (p/when-let [a (tok :A) _ (p/debug-state "a")
                        b (tok :B) _ (p/debug-state "b")]
             (p/result [a b]))
           [:A :B])
        (with-out-str)
        (string/split-lines))
    ["a: (:B)"]

    ))

(deftest debug-parser-t
  (test/are [expr result] (= result expr)

    (-> (p (p/when-let [a (p/debug-parser "a" (tok :A))
                        b (p/debug-parser "b" (tok :B))]
             (p/result [a b]))
           [:A :B :C])
        (with-out-str)
        (string/split-lines))
    ["a: (:A :B :C)"
     "b: (:B :C)"]

    (-> (p (p/when-let [a (p/debug-parser "a" (tok :A))
                        b (p/debug-parser "b" (tok :B))]
             (p/result [a b]))
           [:A :B])
        (with-out-str)
        (string/split-lines))
    ["a: (:A :B)"
     "b: (:B)"]

    (-> (p (p/when-let [a (p/debug-parser "a" (tok :A))
                        b (p/debug-parser "b" (tok :B))]
             (p/result [a b]))
           [:B :C])
        (with-out-str)
        (string/split-lines))
    ["a: (:B :C)"
     "a  backtracked"]

    (-> (p (p/when-let [a (p/debug-parser "a" (tok :A))
                        b (p/debug-parser "b" (tok :B))]
             (p/result [a b]))
           [:A :C])
        (with-out-str)
        (string/split-lines))
    ["a: (:A :C)"
     "b: (:C)"
     "b  backtracked"]

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
