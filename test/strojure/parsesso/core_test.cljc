(ns strojure.parsesso.core-test
  (:require [clojure.string :as string]
            [clojure.test :as test :refer [deftest testing]]
            [strojure.parsesso.core :as p]))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(comment
  (test/run-tests))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;; TODO: Look at pos and remaining input.
(defn- p
  "Parses test input using given parser. Returns custom map with test result."
  [parser input]
  (let [result (p/parse parser input)]
    (cond-> result
      (p/error? result) (assoc :value :<NA>)
      :then,,,,,,,,,,,, (select-keys [:value :consumed]))))

(defn- fail-consumed
  "Returns parser which fails when `p` is successfully consumed."
  [parser]
  (p/alt (p/bind [_ parser] (p/fail "Oops"))
         parser))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest return-t
  (test/are [expr result] (= result expr)
    (p (p/return :A) []) #_$ {:value :A, :consumed false}
    (p (p/return :A) [:B]) #_$ {:value :A, :consumed false}
    (p (fail-consumed (p/return :A)) []) #_$ {:value :A, :consumed false}
    ))

(deftest fail-t
  ;; TODO: Error messages
  (test/are [expr result] (= result expr)
    (p (p/fail "Oops") []) #_$ {:value :<NA>, :consumed false}
    (p (p/fail "Oops") [:A]) #_$ {:value :<NA>, :consumed false}
    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest label-t
  ;; TODO: Test `label`
  )

(deftest token-t
  (test/are [expr result] (= result expr)
    (p (p/token #{:A}) [:A]) #_$ {:value :A, :consumed true}
    (p (p/token #{:A}) [:B]) #_$ {:value :<NA>, :consumed false}
    (p (p/token #{:A}) []) #_$ {:value :<NA>, :consumed false}
    (p (fail-consumed (p/token #{:A})) [:A]) #_$ {:value :<NA>, :consumed true}
    (p (fail-consumed (p/token #{:A})) [:B]) #_$ {:value :<NA>, :consumed false}
    (p (fail-consumed (p/token #{:A})) []) #_$ {:value :<NA>, :consumed false}
    ))

(defn- tok
  [& cs]
  (p/token (set cs)))

(deftest bind-t
  (test/are [expr result] (= result expr)

    (p (p/bind* (tok :A) p/return)
       [:A])
    {:value :A, :consumed true}

    (p (p/bind* (tok :A) (fn [_] (p/fail "Oops")))
       [:A])
    {:value :<NA>, :consumed true}

    (p (p/bind* (tok :A) p/return)
       [:B])
    {:value :<NA>, :consumed false}

    (p (p/bind* (tok :A) (fn [_] (p/fail "Oops")))
       [:B])
    {:value :<NA>, :consumed false}

    (p (p/bind* (tok :A) (fn [_] (tok :B)))
       [:A :B])
    {:value :B, :consumed true}

    (p (p/bind* (tok :A) (fn [_] (tok :B)))
       [:B :A])
    {:value :<NA>, :consumed false}

    (p (p/bind* (tok :A) (fn [_] (tok :B)))
       [:A :A])
    {:value :<NA>, :consumed true}

    ))

(deftest alt-t
  (test/are [expr result] (= result expr)

    (p (p/alt (tok :A)
              (tok :B))
       [:A])
    {:value :A, :consumed true}

    (p (p/alt (tok :A)
              (tok :B))
       [:B])
    {:value :B, :consumed true}

    (p (p/alt (tok :A)
              (tok :B))
       [:C])
    {:value :<NA>, :consumed false}

    (p (p/alt (tok :A)
              (tok :B))
       [])
    {:value :<NA>, :consumed false}

    (p (p/alt (fail-consumed (tok :A))
              (tok :B))
       [:A])
    {:value :<NA>, :consumed true}

    (p (p/alt (fail-consumed (tok :A))
              (tok :B))
       [:B])
    {:value :B, :consumed true}

    (p (p/alt (fail-consumed (tok :A))
              (tok :B))
       [:C])
    {:value :<NA>, :consumed false}

    (p (p/alt (fail-consumed (tok :A))
              (tok :B))
       [])
    {:value :<NA>, :consumed false}

    (p (p/alt (tok :A)
              (fail-consumed (tok :B)))
       [:A])
    {:value :A, :consumed true}

    (p (p/alt (tok :A)
              (fail-consumed (tok :B)))
       [:B])
    {:value :<NA>, :consumed true}

    (p (p/alt (tok :A)
              (fail-consumed (tok :B)))
       [:C])
    {:value :<NA>, :consumed false}

    (p (p/alt (tok :A)
              (fail-consumed (tok :B)))
       [])
    {:value :<NA>, :consumed false}

    ))

(deftest trim-t
  (test/are [expr result] (= result expr)

    (p (p/trim (tok :A))
       [:A])
    {:value :A, :consumed true}

    (p (p/trim (tok :A))
       [:B])
    {:value :<NA>, :consumed false}

    (p (p/trim (tok :A))
       [])
    {:value :<NA>, :consumed false}

    (p (p/trim (fail-consumed (tok :A)))
       [:A])
    {:value :<NA>, :consumed false}

    (p (p/trim (fail-consumed (tok :A)))
       [:B])
    {:value :<NA>, :consumed false}

    (p (p/trim (fail-consumed (tok :A)))
       [])
    {:value :<NA>, :consumed false}

    ))

(deftest look-ahead-t
  (test/are [expr result] (= result expr)

    (p (p/look-ahead (tok :A))
       [:A])
    {:value :A, :consumed false}

    (p (p/look-ahead (tok :A))
       [:B])
    {:value :<NA>, :consumed false}

    (p (p/look-ahead (tok :A))
       [])
    {:value :<NA>, :consumed false}

    (p (p/look-ahead (fail-consumed (tok :A)))
       [:A])
    {:value :<NA>, :consumed true}

    (p (p/look-ahead (fail-consumed (tok :A)))
       [:B])
    {:value :<NA>, :consumed false}

    (p (p/look-ahead (fail-consumed (tok :A)))
       [])
    {:value :<NA>, :consumed false}

    ))

(deftest option-t
  (test/are [expr result] (= result expr)

    (p (p/option (tok :A) :X)
       [:A])
    {:value :A, :consumed true}

    (p (p/option (tok :A) :X)
       [:B])
    {:value :X, :consumed false}

    (p (p/option (tok :A) :X)
       [])
    {:value :X, :consumed false}

    (p (p/option (fail-consumed (tok :A)) :X)
       [:A])
    {:value :<NA>, :consumed true}

    (p (p/option (fail-consumed (tok :A)) :X)
       [:B])
    {:value :X, :consumed false}

    (p (p/option (fail-consumed (tok :A)) :X)
       [])
    {:value :X, :consumed false}

    ))

(deftest optional-t
  (test/are [expr result] (= result expr)

    (p (p/optional (tok :A))
       [:A])
    {:value nil, :consumed true}

    (p (p/optional (tok :A))
       [:B])
    {:value nil, :consumed false}

    (p (p/optional (tok :A))
       [])
    {:value nil, :consumed false}

    (p (p/optional (fail-consumed (tok :A)))
       [:A])
    {:value :<NA>, :consumed true}

    (p (p/optional (fail-consumed (tok :A)))
       [:B])
    {:value nil, :consumed false}

    (p (p/optional (fail-consumed (tok :A)))
       [])
    {:value nil, :consumed false}

    ))

(deftest between-t
  (test/are [expr result] (= result expr)

    (p (p/between (tok :A) (tok :L) (tok :R))
       [:L :A :R])
    {:value :A, :consumed true}

    (p (p/between (tok :A) (tok :L) (tok :R))
       [:R :A :L])
    {:value :<NA>, :consumed false}

    (p (p/between (tok :A) (tok :L) (tok :R))
       [:L :A])
    {:value :<NA>, :consumed true}

    (p (p/between (tok :A) (tok :L) (tok :R))
       [:A :R])
    {:value :<NA>, :consumed false}

    (p (p/between (tok :A) (tok :L) (tok :R))
       [:A])
    {:value :<NA>, :consumed false}

    (p (p/between (tok :A) (tok :L) (tok :R))
       [])
    {:value :<NA>, :consumed false}

    (p (p/between (tok :A) (tok :I))
       [:I :A :I])
    {:value :A, :consumed true}

    (p (p/between (tok :A) (tok :I))
       [:I :A])
    {:value :<NA>, :consumed true}

    (p (p/between (tok :A) (tok :I))
       [:A :I])
    {:value :<NA>, :consumed false}

    (p (p/between (tok :A) (tok :I))
       [:A])
    {:value :<NA>, :consumed false}

    (p (p/between (tok :A) (tok :I))
       [])
    {:value :<NA>, :consumed false}

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;; TODO: Test `many` for very long input.
(deftest many-t

  (testing 'p/many*
    (test/are [expr result] (= result expr)

      (p (p/many* (tok :A :B :C))
         [:A :B :C :D :E :F])
      {:value [:A :B :C], :consumed true}

      (p (p/many* (tok :D :E :F))
         [:A :B :C :D :E :F])
      {:value [], :consumed false}

      (p (p/many* (tok :A :B :C))
         [])
      {:value [], :consumed false}

      ))

  (testing 'p/many+
    (test/are [expr result] (= result expr)

      (p (p/many+ (tok :A :B :C))
         [:A :B :C :D :E :F])
      {:value [:A :B :C], :consumed true}

      (p (p/many+ (tok :D :E :F))
         [:A :B :C :D :E :F])
      {:value :<NA>, :consumed false}

      (p (p/many+ (tok :A :B :C))
         [])
      {:value :<NA>, :consumed false}

      )))

(deftest many-count-t
  (test/are [expr result] (= result expr)

    (p (p/many-count 3 (tok :A1 :A2 :A3))
       [:A1 :A2 :A3])
    {:value [:A1 :A2 :A3], :consumed true}

    (p (p/many-count 3 (tok :A1 :A2 :A3))
       [:A1 :A2 :A3 :A4])
    {:value [:A1 :A2 :A3], :consumed true}

    (p (p/many-count 3 (tok :A1 :A2 :A3))
       [:A1 :A2 :A3 :B])
    {:value [:A1 :A2 :A3], :consumed true}

    (p (p/many-count 3 (tok :A1 :A2 :A3))
       [:A1 :A2])
    {:value :<NA>, :consumed true}

    (p (p/many-count 3 (tok :A1 :A2 :A3))
       [:A1 :A2 :B])
    {:value :<NA>, :consumed true}

    (p (p/many-count 3 (tok :A1 :A2 :A3))
       [:B :A1 :A2 :A3])
    {:value :<NA>, :consumed false}

    (p (p/many-count 3 (tok :A1 :A2 :A3))
       [:B :A1])
    {:value :<NA>, :consumed false}

    (p (p/many-count 3 (tok :A1 :A2 :A3))
       [])
    {:value :<NA>, :consumed false}

    ))

(deftest skip-t

  (testing 'p/skip*
    (test/are [expr result] (= result expr)

      (p (p/skip* (tok :A))
         [:A :A :A :B :B :B])
      {:value nil, :consumed true}

      (p (p/skip* (tok :A))
         [:B :B :B])
      {:value nil, :consumed false}

      (p (p/skip* (tok :A))
         [])
      {:value nil, :consumed false}

      ))

  (testing 'p/skip+
    (test/are [expr result] (= result expr)

      (p (p/skip+ (tok :A))
         [:A :A :A :B :B :B])
      {:value nil, :consumed true}

      (p (p/skip+ (tok :A))
         [:B :B :B])
      {:value :<NA>, :consumed false}

      (p (p/skip+ (tok :A))
         [])
      {:value :<NA>, :consumed false}

      )))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest sep-by-t

  (testing 'p/sep-by*
    (test/are [expr result] (= result expr)

      (p (p/sep-by* (tok :A) (tok :S))
         [:A :S :A :S :A])
      {:value [:A :A :A], :consumed true}

      (p (p/sep-by* (tok :A) (tok :S))
         [:A :S :A :S :A :S])
      {:value :<NA>, :consumed true}

      (p (p/sep-by* (tok :A) (tok :S))
         [:A :S :A :S :A :B])
      {:value [:A :A :A], :consumed true}

      (p (p/sep-by* (tok :A) (tok :S))
         [])
      {:value nil, :consumed false}

      (p (p/sep-by* (tok :A) (tok :S))
         [:B])
      {:value nil, :consumed false}

      (p (p/sep-by* (tok :A) (tok :S))
         [:S])
      {:value nil, :consumed false}

      ))

  (testing 'p/sep-by+
    (test/are [expr result] (= result expr)

      (p (p/sep-by+ (tok :A) (tok :S))
         [:A :S :A :S :A])
      {:value [:A :A :A], :consumed true}

      (p (p/sep-by+ (tok :A) (tok :S))
         [:A :S :A :S :A :S])
      {:value :<NA>, :consumed true}

      (p (p/sep-by+ (tok :A) (tok :S))
         [:A :S :A :S :A :B])
      {:value [:A :A :A], :consumed true}

      (p (p/sep-by+ (tok :A) (tok :S))
         [])
      {:value :<NA>, :consumed false}

      (p (p/sep-by+ (tok :A) (tok :S))
         [:B])
      {:value :<NA>, :consumed false}

      (p (p/sep-by+ (tok :A) (tok :S))
         [:S])
      {:value :<NA>, :consumed false}

      )))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest many-till-t
  (test/are [expr result] (= result expr)

    (p (p/many-till (tok :A1 :A2 :A3)
                    (tok :END))
       [:A1 :A2 :A3 :END])
    {:value [:A1 :A2 :A3], :consumed true}

    (p (p/many-till (tok :A1 :A2 :A3)
                    (tok :END))
       [:A1 :A2 :A3 :B :END])
    {:value :<NA>, :consumed true}

    (p (p/many-till (tok :A1 :A2 :A3)
                    (tok :END))
       [:B :END])
    {:value :<NA>, :consumed false}

    (p (p/many-till (tok :A1 :A2 :A3)
                    (tok :END))
       [:END])
    {:value nil, :consumed true}

    (p (p/many-till (p/alt (tok :A1 :A2 :A3)
                           (p/many-till (tok :B1 :B2 :B3)
                                        (tok :END)))
                    (tok :END))
       [:A1 :A2 :A3 :B1 :B2 :B3 :END :A1 :A2 :A3 :END])
    {:value [:A1 :A2 :A3 [:B1 :B2 :B3] :A1 :A2 :A3], :consumed true}

    (p (p/many-till (tok :A1 :A2 :A3)
                    (tok :END))
       (concat (take 10000 (cycle [:A1 :A2 :A3])) [:END]))
    {:value (take 10000 (cycle [:A1 :A2 :A3])), :consumed true}

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftest debug-state-t
  (test/are [expr result] (= result expr)

    (-> (p (p/bind [_ (p/debug-state "a") a (tok :A)
                    _ (p/debug-state "b") b (tok :B)]
             [a b])
           [:A :B :C])
        (with-out-str)
        (string/split-lines))
    ["a: [:A :B :C]"
     "b: [:B :C]"]

    (-> (p (p/bind [_ (p/debug-state "a") a (tok :A)
                    _ (p/debug-state "b") b (tok :B)]
             [a b])
           [:A :B])
        (with-out-str)
        (string/split-lines))
    ["a: [:A :B]"
     "b: [:B]"]

    (-> (p (p/bind [a (tok :A) _ (p/debug-state "a")
                    b (tok :B) _ (p/debug-state "b")]
             [a b])
           [:A :B])
        (with-out-str)
        (string/split-lines))
    ["a: [:B]"]

    ))

(deftest debug-parser-t
  (test/are [expr result] (= result expr)

    (-> (p (p/bind [a (p/debug-parser "a" (tok :A))
                    b (p/debug-parser "b" (tok :B))]
             [a b])
           [:A :B :C])
        (with-out-str)
        (string/split-lines))
    ["a: [:A :B :C]"
     "a  backtracked"
     "b: [:B :C]"
     "b  backtracked"]

    (-> (p (p/bind [a (p/debug-parser "a" (tok :A))
                    b (p/debug-parser "b" (tok :B))]
             [a b])
           [:A :B])
        (with-out-str)
        (string/split-lines))
    ["a: [:A :B]"
     "a  backtracked"
     "b: [:B]"
     "b  backtracked"]

    ))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
