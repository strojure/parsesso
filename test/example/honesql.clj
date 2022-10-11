(ns example.honesql
  "Demo: Parse SQL SELECT query to HoneySQL data structures."
  {:clj-kondo/config '{:linters {:missing-docstring {:level :off}}}}
  (:require [strojure.parsesso.char.core :as char]
            [strojure.parsesso.core :as p]))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(comment
  "SELECT u.username, s.name FROM user AS u, status AS s WHERE (u.statusid = s.id) AND (u.id = ?)"

  {:select [:u.username :s.name]
   :from [[:user :u] [:status :s]]
   :where [:and [:= :u.statusid :s.id]
           [:= :u.id 9]]}

  "SELECT username, name FROM user, status WHERE (user.statusid = status.id) AND (user.id = ?)"

  {:select [:username :name]
   :from [:user :status]
   :where [:and [:= :user.statusid :status.id]
           [:= :user.id 9]]})

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(def table-id
  (p/many-some char/alpha?))

(def table-ref
  (p/maybe (p/tuple table-id (char/one-of? "."))))

(def column-id
  (p/many-some char/alpha?))

(def alias-id
  (p/many-some char/alpha?))

(def skip-opt-ws
  (p/skip-zero char/whitespace?))

(def skip-some-ws
  (p/skip-some char/whitespace?))

(def comma-sep
  (p/maybe (-> (char/one-of? ",")
               (p/between skip-opt-ws))))

(defn sep-by-comma
  [p]
  (p/sep-by-some p comma-sep))

(def as-expr
  (p/after (p/maybe (p/between (p/word "as" :i) skip-some-ws))
           alias-id))

(def select-expr
  (sep-by-comma (p/tuple (p/optional table-ref)
                         column-id
                         (p/optional as-expr))))

(def from-clause
  (sep-by-comma table-id))

(def select-parser
  (p/bind-let [_ (p/maybe (p/after (p/word "select" :i) skip-some-ws))
               expr-xs select-expr
               _ (p/between (p/word "from" :i) skip-some-ws)
               from-xs from-clause]
    (p/result {:select (into [] (map (fn [[t id as]]
                                       (let [col (keyword (char/++ [t id]))]
                                         (if as
                                           [col (keyword (char/++ as))]
                                           col))))
                             expr-xs)
               :from (into [] (map (comp keyword char/++)) from-xs)})))

(comment
  (def -q "SELECT username, u.name AS x FROM user, status")
  (p/parse select-parser -q)
  #_{:select [:username [:u.name :x]], :from [:user :status]}
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
