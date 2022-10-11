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

(def skip-opt-ws (p/skip-zero char/whitespace?))

(def skip-some-ws (p/skip-some char/whitespace?))

(defn sep-by-comma
  "Parses `p` separated by commas."
  [p]
  (p/sep-by-some p (p/maybe (-> (char/one-of? ",")
                                (p/between skip-opt-ws)))))

(def table-name
  "Parses table name as `:table`."
  (-> (p/many-some char/alpha?)
      (p/with char/++ keyword)))

(def column-name
  "Parses column as `:column` or `:table.column`."
  (-> (p/tuple (p/optional (p/maybe (p/tuple (p/many-some char/alpha?)
                                             (char/one-of? "."))))
               (p/many-some char/alpha?))
      (p/with char/++ keyword)))

(def alias-id
  "Parses alias keyword as `:alias`."
  (-> (p/many-some char/alpha?)
      (p/with char/++ keyword)))

(def as-expr
  (->> alias-id (p/after (p/maybe (-> (p/word "as" :i)
                                      (p/between skip-some-ws))))))

(def column-alias
  "Parses column ID with optional alias like `:column` or `[:column :alias]`."
  (-> (p/tuple column-name (p/optional as-expr))
      (p/with (fn [[col as]] (if as [col as] col)))))

(def select-statement
  "Parses SQL SELECT statement to `{:select [...] :from [...] ...}`."
  (p/bind-let [_ (p/maybe (p/after (p/word "select" :i) skip-some-ws))
               select (sep-by-comma column-alias)
               _ (-> (p/word "from" :i) (p/between skip-some-ws))
               from (sep-by-comma table-name)]
    (p/result
      {:select (vec select)
       :from (vec from)})))

(comment
  (def -q "SELECT username, u.name AS x FROM user, status")
  (p/parse select-statement -q)
  #_{:select [:username [:u.name :x]], :from [:user :status]}
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
