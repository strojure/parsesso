(ns strojure.parsesso.parser.state
  (:require [strojure.parsesso.core :as p]
            [strojure.parsesso.impl.reply :as reply]
            [strojure.parsesso.impl.state :as state]))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn do-update-parser-state
  "This parser applies function `f` to the parser state and returns `nil`."
  ([f]
   (p/parser
     (fn [state context]
       (let [s (f state)]
         (reply/e-ok context s nil)))))
  ([f arg]
   (do-update-parser-state #(f % arg))))

(defn update-parser-state
  "This parser applies function `f` to the parser state and returns modified
  parser state."
  ([f]
   (p/parser
     (fn [state context]
       (let [s (f state)]
         (reply/e-ok context s s)))))
  ([f arg]
   (update-parser-state #(f % arg)))
  ([f arg & args]
   (update-parser-state #(apply f % arg args))))

(def get-parser-state
  "This parser returns the full parser state as a 'State' record."
  (update-parser-state identity))

(def ^{:arglists '([state])}
  set-parser-state
  "This parser set the full parser state to `state`."
  (comp do-update-parser-state constantly))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(def get-input
  "This parser returns the current input."
  (p/fmap state/input get-parser-state))

(def ^{:arglists '([input])}
  set-input
  "This parser continues parsing with `input`."
  (partial do-update-parser-state state/set-input))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(def get-user-state
  "This parser returns the current user state."
  (p/fmap state/user get-parser-state))

(def ^{:arglists '([u])}
  set-user-state
  "This parser sets the user state to `u`"
  (partial do-update-parser-state state/set-user-state))

(defn update-user-state
  "This parser applies function `f` to the user state. Suppose that we want to
  count identifiers in a source, we could use the user state as:

      (bind-let [x identifier
                 _ (update-user-state inc)]
        (result x))
  "
  ([f]
   (do-update-parser-state state/update-user-state f))
  ([f & args]
   (do-update-parser-state state/update-user-state #(apply f % args))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
