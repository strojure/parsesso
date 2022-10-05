(ns strojure.parsesso.impl.reply
  (:refer-clojure :exclude [replace]))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftype Context [cok, eok, cerr, eerr])

(defn c-ok
  "Replies with value as consumed (consumed-ok)."
  [^Context context, state, x]
  ((.-cok context) state x))

(defn e-ok
  "Replies with value as not consumed (empty-ok)."
  [^Context context, state, x]
  ((.-eok context) state x))

(defn c-err
  "Fails with error as consumed (consumed-error)."
  [^Context context, error]
  ((.-cerr context) error))

(defn e-err
  "Fails with error as not consumed (empty-error)."
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
  [context m]
  (assert (map? m))
  (let [m (update-keys m (comp eval eval))]
    (assert (every? #{c-ok e-ok c-err e-err} (keys m)))
    `(replace* ~context ~(m c-ok) ~(m e-ok) ~(m c-err) ~(m e-err))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defrecord Result [value consumed state])

(defrecord Failure [consumed error])

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn new-context
  []
  (Context. (fn consumed-ok, [s x] (Result. x true s))
            (fn empty-ok,,,, [s x] (Result. x false s))
            (fn consumed-err [e],,,, (Failure. true e))
            (fn empty-err,,, [e],,,, (Failure. false e))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
