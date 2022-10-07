(ns strojure.parsesso.impl.reply
  (:refer-clojure :exclude [replace]))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftype Context [cok, eok, cerr, eerr])

(defn c-ok
  "Replies with result value as consumed (consumed-ok)."
  [^Context context, state, x]
  ((.-cok context) state x))

(defn e-ok
  "Replies with result value as not consumed (empty-ok)."
  [^Context context, state, x]
  ((.-eok context) state x))

(defn c-err
  "Fails with parser error as consumed (consumed-error)."
  [^Context context, error]
  ((.-cerr context) error))

(defn e-err
  "Fails with parser error as not consumed (empty-error)."
  [^Context context, error]
  ((.-eerr context) error))

(defn replace*
  "Returns new instance of context with replaced functions, nil arg keep
  functions untouched. To be used with macro."
  [^Context context, -c-ok, -e-ok, -c-err, -e-err]
  (Context. (or -c-ok (.-cok context))
            (or -e-ok (.-eok context))
            (or -c-err (.-cerr context))
            (or -e-err (.-eerr context))))

(defmacro replace
  "Expands to code updating specified context functions at once."
  [context m]
  (assert (map? m))
  (let [m (update-keys m (comp eval eval))]
    (assert (every? #{c-ok e-ok c-err e-err} (keys m)))
    `(replace* ~context ~(m c-ok) ~(m e-ok) ~(m c-err) ~(m e-err))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defrecord Result [value consumed state])

(defrecord Failure [consumed error])

(def ^{:arglists '([reply])} result?
  "True if `reply` is parsing result with value."
  (partial instance? Result))

(def ^{:arglists '([reply])} error?
  "True if `reply` is parser error."
  (partial instance? Failure))

(defn value
  "Returns value for Result reply or throws exception otherwise."
  [reply]
  (cond
    (result? reply) (:value reply)
    (error? reply) (throw (ex-info (str (:error reply)) reply))
    :else (throw (ex-info "Invalid parser reply" {::reply reply}))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn new-context
  "Returns new instance of context with initialized reply functions."
  []
  (Context. (fn c-ok [s x] (Result. x true s))
            (fn e-ok [s x] (Result. x false s))
            (fn c-err [e] (Failure. true e))
            (fn e-err [e] (Failure. false e))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
