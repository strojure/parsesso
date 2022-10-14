(ns demo.honeysql-select
  "Demo: Parse SQL SELECT query to HoneySQL data structures."
  {:clj-kondo/config '{:linters {:missing-docstring {:level :off}}}}
  (:require [strojure.parsesso.char :as char]
            [strojure.parsesso.parser :as p]))

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
      (p/value char/str* keyword)))

(def column-name
  "Parses column as `:column` or `:table.column`."
  (-> (p/group (p/option (p/maybe (p/group (p/many1 char/letter?)
                                           (char/is "."))))
               (p/many1 char/letter?))
      (p/value char/str* keyword)))

(comment
  (p/parse column-name "username") #_=> :username
  (p/parse column-name "u.username") #_=> :u.username
  (p/parse column-name "u.u.username") #_=> :u.u
  )

(def as-expr
  "Parses alias keyword like `:alias` after AS."
  (p/after (p/maybe (-> (p/word "as" :ic) (p/between space1)))
           (-> (p/many1 char/letter?)
               (p/value char/str* keyword))))

(comment
  (p/parse as-expr " AS name") #_=> :name
  )

(defn with-as
  "Parses `p` with optional alias like `:name` or `[:name :alias]`."
  [p]
  (-> (p/group p (p/option as-expr))
      (p/value (fn [[x as]] (if as [x as] x)))))

(comment
  (p/parse (with-as column-name) "u.username") #_=> :u.username
  (p/parse (with-as column-name) "u.username AS name") #_=> [:u.username :name]
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(def select-statement
  "Parses SQL SELECT statement to `{:select [...] :from [...] ...}`."
  (p/bind-let [_ (p/maybe (p/after (p/word "select" :ic) space1))
               select (comma-sep (with-as column-name))
               _ (-> (p/word "from" :ic) (p/between space1))
               from (comma-sep (with-as table-name))]
    (p/result
      {:select (vec select)
       :from (vec from)})))

(comment
  (def -q "SELECT username, u.name AS x FROM user AS u, status")
  (p/parse select-statement -q)
  #_=> {:select [:username [:u.name :x]],
        :from [[:user :u] :status]}
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
