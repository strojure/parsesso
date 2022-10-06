(ns strojure.parsesso.expr-test
  (:require [clojure.string :as string]
            [clojure.test :as test :refer [deftest]]
            [strojure.parsesso.core :as p]
            [strojure.parsesso.expr :as expr]))

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

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest chain-left-some-t
  (test/are [expr result] (= result expr)

    (p (expr/chain-left-some (tok 1 2 3 3 4 5 6 7 8 9)
                             (tok + - * /))
       [8 - 2 / 2])
    {:consumed true, :value 3}

    (p (expr/chain-left-some (tok 1 2 3 3 4 5 6 7 8 9)
                             (tok + - * /))
       [8 - 2 2])
    {:consumed true, :value 6}

    (p (expr/chain-left-some (tok 1 2 3 3 4 5 6 7 8 9)
                             (tok + - * /))
       [1])
    {:consumed true, :value 1}

    (p (expr/chain-left-some (tok 1 2 3 3 4 5 6 7 8 9)
                             (tok + - * /))
       [+])
    {:consumed false, :error ["error at index 0:"
                              (str "unexpected " (pr-str +))]}

    (p (expr/chain-left-some (tok 1 2 3 3 4 5 6 7 8 9)
                             (tok + - * /))
       [0])
    {:consumed false, :error ["error at index 0:"
                              "unexpected 0"]}

    (p (expr/chain-left-some (tok 1 2 3 3 4 5 6 7 8 9)
                             (tok + - * /))
       [])
    {:consumed false, :error ["error at index 0:"
                              "unexpected end of input"]}

    ))

(deftest chain-left-zero-t
  (test/are [expr result] (= result expr)

    (p (expr/chain-left-zero (tok 1 2 3 3 4 5 6 7 8 9)
                             (tok + - * /)
                             0)
       [8 - 2 / 2])
    {:consumed true, :value 3}

    (p (expr/chain-left-zero (tok 1 2 3 3 4 5 6 7 8 9)
                             (tok + - * /)
                             0)
       [8 - 2 2])
    {:consumed true, :value 6}

    (p (expr/chain-left-zero (tok 1 2 3 3 4 5 6 7 8 9)
                             (tok + - * /)
                             0)
       [1])
    {:consumed true, :value 1}

    (p (expr/chain-left-zero (tok 1 2 3 3 4 5 6 7 8 9)
                             (tok + - * /)
                             0)
       [+])
    {:consumed false, :value 0}

    (p (expr/chain-left-zero (tok 1 2 3 3 4 5 6 7 8 9)
                             (tok + - * /)
                             0)
       [0])
    {:consumed false, :value 0}

    (p (expr/chain-left-zero (tok 1 2 3 3 4 5 6 7 8 9)
                             (tok + - * /)
                             0)
       [])
    {:consumed false, :value 0}

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest chain-right-some-t
  (test/are [expr result] (= result expr)

    (p (expr/chain-right-some (tok 1 2 3 3 4 5 6 7 8 9)
                              (tok + - * /))
       [8 - 2 / 2])
    {:consumed true, :value 7}

    (p (expr/chain-right-some (tok 1 2 3 3 4 5 6 7 8 9)
                              (tok + - * /))
       [8 - 2 2])
    {:consumed true, :value 6}

    (p (expr/chain-right-some (tok 1 2 3 3 4 5 6 7 8 9)
                              (tok + - * /))
       [1])
    {:consumed true, :value 1}

    (p (expr/chain-right-some (tok 1 2 3 3 4 5 6 7 8 9)
                              (tok + - * /))
       [+])
    {:consumed false, :error ["error at index 0:"
                              (str "unexpected " (pr-str +))]}

    (p (expr/chain-right-some (tok 1 2 3 3 4 5 6 7 8 9)
                              (tok + - * /))
       [0])
    {:consumed false, :error ["error at index 0:"
                              "unexpected 0"]}

    (p (expr/chain-right-some (tok 1 2 3 3 4 5 6 7 8 9)
                              (tok + - * /))
       [])
    {:consumed false, :error ["error at index 0:"
                              "unexpected end of input"]}

    ))

(deftest chain-right-zero-t
  (test/are [expr result] (= result expr)

    (p (expr/chain-right-zero (tok 1 2 3 3 4 5 6 7 8 9)
                              (tok + - * /)
                              0)
       [8 - 2 / 2])
    {:consumed true, :value 7}

    (p (expr/chain-right-zero (tok 1 2 3 3 4 5 6 7 8 9)
                              (tok + - * /)
                              0)
       [8 - 2 2])
    {:consumed true, :value 6}

    (p (expr/chain-right-zero (tok 1 2 3 3 4 5 6 7 8 9)
                              (tok + - * /)
                              0)
       [1])
    {:consumed true, :value 1}

    (p (expr/chain-right-zero (tok 1 2 3 3 4 5 6 7 8 9)
                              (tok + - * /)
                              0)
       [+])
    {:consumed false, :value 0}

    (p (expr/chain-right-zero (tok 1 2 3 3 4 5 6 7 8 9)
                              (tok + - * /)
                              0)
       [0])
    {:consumed false, :value 0}

    (p (expr/chain-right-zero (tok 1 2 3 3 4 5 6 7 8 9)
                              (tok + - * /)
                              0)
       [])
    {:consumed false, :value 0}

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
