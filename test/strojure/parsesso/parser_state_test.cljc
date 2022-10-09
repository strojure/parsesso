(ns strojure.parsesso.parser-state-test
  (:require [clojure.test :as test :refer [deftest]]
            [strojure.parsesso.core :as p]
            [strojure.parsesso.parser-state :as state]))

#_(test/run-tests)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest get-parser-state-t
  (test/are [expr result] (= result expr)

    (-> (p/parse* state/get-parser-state [:A])
        ((juxt (comp :input :value) (comp :input :state))))
    ['(:A) '(:A)]

    ))

(deftest set-parser-state-t
  (test/are [expr result] (= result expr)

    (-> (p/parse* (state/set-parser-state ::state) [:A])
        :state)
    ::state

    ))

(deftest update-parser-state-t
  (test/are [expr result] (= result expr)

    (-> (p/parse* (state/update-parser-state :input) [:A])
        :state)
    '(:A)

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest get-input-t
  (test/are [expr result] (= result expr)

    (-> (p/parse* state/get-input [:A])
        :value)
    '(:A)

    ))

(deftest set-input-t
  (test/are [expr result] (= result expr)

    (-> (p/parse* (state/set-input [:B]) [:A])
        :state :input)
    '(:B)

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest get-user-state-t
  (test/are [expr result] (= result expr)

    (-> (p/parse* (p/after (state/set-user-state ::user) state/get-user-state) [:A])
        :value)
    ::user

    ))

(deftest set-user-state-t
  (test/are [expr result] (= result expr)

    (-> (p/parse* (state/set-user-state ::state) [:A])
        :state :user)
    ::state

    ))

(deftest update-user-state-t
  (test/are [expr result] (= result expr)

    (-> (p/parse* (state/update-user-state (constantly ::state)) [:A])
        :state :user)
    ::state

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
