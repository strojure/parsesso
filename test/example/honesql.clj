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

(def space0 (p/skip0 char/white?))

(def space1 (p/skip1 char/white?))

(defn comma-sep
  "Parses `p` separated by commas."
  [p]
  (p/sep1 p (p/maybe (-> (char/is ",")
                         (p/between space0)))))

(def table-name
  "Parses table name as `:table`."
  (-> (p/many1 char/letter?)
      (p/using char/str* keyword)))

(def column-name
  "Parses column as `:column` or `:table.column`."
  (-> (p/tuple (p/option (p/maybe (p/tuple (p/many1 char/letter?)
                                           (char/is "."))))
               (p/many1 char/letter?))
      (p/using char/str* keyword)))

(def alias-id
  "Parses alias keyword as `:alias`."
  (-> (p/many1 char/letter?)
      (p/using char/str* keyword)))

(def as-expr
  (->> alias-id (p/after (p/maybe (-> (p/word "as" :ic)
                                      (p/between space1))))))

(def column-alias
  "Parses column ID with optional alias like `:column` or `[:column :alias]`."
  (-> (p/tuple column-name (p/option as-expr))
      (p/using (fn [[col as]] (if as [col as] col)))))

(def select-statement
  "Parses SQL SELECT statement to `{:select [...] :from [...] ...}`."
  (p/bind-let [_ (p/maybe (p/after (p/word "select" :ic) space1))
               select (comma-sep column-alias)
               _ (-> (p/word "from" :ic) (p/between space1))
               from (comma-sep table-name)]
    (p/result
      {:select (vec select)
       :from (vec from)})))

(comment
  (def -q "SELECT username, u.name AS x FROM user, status")
  (p/parse select-statement -q)
  #_{:select [:username [:u.name :x]], :from [:user :status]}
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
