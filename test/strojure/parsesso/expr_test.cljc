(ns strojure.parsesso.expr-test
  (:require [clojure.string :as string]
            [clojure.test :as test :refer [deftest]]
            [strojure.parsesso.expr :as expr]
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

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest chain1-left-t
  (test/are [expr result] (= result expr)

    (p (expr/chain1-left (tok 1 2 3 3 4 5 6 7 8 9)
                         (tok + - * /))
       [8 - 2 / 2])
    {:consumed true, :value 3}

    (p (expr/chain1-left (tok 1 2 3 3 4 5 6 7 8 9)
                         (tok + - * /))
       [8 - 2 2])
    {:consumed true, :value 6}

    (p (expr/chain1-left (tok 1 2 3 3 4 5 6 7 8 9)
                         (tok + - * /))
       [1])
    {:consumed true, :value 1}

    (p (expr/chain1-left (tok 1 2 3 3 4 5 6 7 8 9)
                         (tok + - * /))
       [+])
    {:consumed false, :error ["error at index 0:"
                              (str "unexpected " (p/render +))]}

    (p (expr/chain1-left (tok 1 2 3 3 4 5 6 7 8 9)
                         (tok + - * /))
       [0])
    {:consumed false, :error ["error at index 0:"
                              "unexpected 0"]}

    (p (expr/chain1-left (tok 1 2 3 3 4 5 6 7 8 9)
                         (tok + - * /))
       [])
    {:consumed false, :error ["error at index 0:"
                              "unexpected end of input"]}

    ))

(deftest chain0-left-t
  (test/are [expr result] (= result expr)

    (p (expr/chain0-left (tok 1 2 3 3 4 5 6 7 8 9)
                         (tok + - * /)
                         0)
       [8 - 2 / 2])
    {:consumed true, :value 3}

    (p (expr/chain0-left (tok 1 2 3 3 4 5 6 7 8 9)
                         (tok + - * /)
                         0)
       [8 - 2 2])
    {:consumed true, :value 6}

    (p (expr/chain0-left (tok 1 2 3 3 4 5 6 7 8 9)
                         (tok + - * /)
                         0)
       [1])
    {:consumed true, :value 1}

    (p (expr/chain0-left (tok 1 2 3 3 4 5 6 7 8 9)
                         (tok + - * /)
                         0)
       [+])
    {:consumed false, :value 0}

    (p (expr/chain0-left (tok 1 2 3 3 4 5 6 7 8 9)
                         (tok + - * /)
                         0)
       [0])
    {:consumed false, :value 0}

    (p (expr/chain0-left (tok 1 2 3 3 4 5 6 7 8 9)
                         (tok + - * /)
                         0)
       [])
    {:consumed false, :value 0}

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest chain1-right-t
  (test/are [expr result] (= result expr)

    (p (expr/chain1-right (tok 1 2 3 3 4 5 6 7 8 9)
                          (tok + - * /))
       [8 - 2 / 2])
    {:consumed true, :value 7}

    (p (expr/chain1-right (tok 1 2 3 3 4 5 6 7 8 9)
                          (tok + - * /))
       [8 - 2 2])
    {:consumed true, :value 6}

    (p (expr/chain1-right (tok 1 2 3 3 4 5 6 7 8 9)
                          (tok + - * /))
       [1])
    {:consumed true, :value 1}

    (p (expr/chain1-right (tok 1 2 3 3 4 5 6 7 8 9)
                          (tok + - * /))
       [+])
    {:consumed false, :error ["error at index 0:"
                              (str "unexpected " (p/render +))]}

    (p (expr/chain1-right (tok 1 2 3 3 4 5 6 7 8 9)
                          (tok + - * /))
       [0])
    {:consumed false, :error ["error at index 0:"
                              "unexpected 0"]}

    (p (expr/chain1-right (tok 1 2 3 3 4 5 6 7 8 9)
                          (tok + - * /))
       [])
    {:consumed false, :error ["error at index 0:"
                              "unexpected end of input"]}

    ))

(deftest chain0-right-t
  (test/are [expr result] (= result expr)

    (p (expr/chain0-right (tok 1 2 3 3 4 5 6 7 8 9)
                          (tok + - * /)
                          0)
       [8 - 2 / 2])
    {:consumed true, :value 7}

    (p (expr/chain0-right (tok 1 2 3 3 4 5 6 7 8 9)
                          (tok + - * /)
                          0)
       [8 - 2 2])
    {:consumed true, :value 6}

    (p (expr/chain0-right (tok 1 2 3 3 4 5 6 7 8 9)
                          (tok + - * /)
                          0)
       [1])
    {:consumed true, :value 1}

    (p (expr/chain0-right (tok 1 2 3 3 4 5 6 7 8 9)
                          (tok + - * /)
                          0)
       [+])
    {:consumed false, :value 0}

    (p (expr/chain0-right (tok 1 2 3 3 4 5 6 7 8 9)
                          (tok + - * /)
                          0)
       [0])
    {:consumed false, :value 0}

    (p (expr/chain0-right (tok 1 2 3 3 4 5 6 7 8 9)
                          (tok + - * /)
                          0)
       [])
    {:consumed false, :value 0}

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
