(ns strojure.parsesso.impl.reply
  (:refer-clojure :exclude [update]))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftype Context [cok, eok, cerr, eerr])

(defn c-ok
  "Replies with value as consumed (consumed-ok)."
  [^Context c x s e]
  ((.-cok c) x s e))

(defn e-ok
  "Replies with value as not consumed (empty-ok)."
  [^Context c x s e]
  ((.-eok c) x s e))

(defn c-err
  "Fails with error as consumed (consumed-error)."
  [^Context c e]
  ((.-cerr c) e))

(defn e-err
  "Fails with error as not consumed (empty-error)."
  [^Context c e]
  ((.-eerr c) e))

(defn update-context
  "Returns new instance of context with replaced functions, nil arg keep
  functions untouched. To be used with macro."
  [^Context context -c-ok, -e-ok, -c-err, -e-err]
  (Context. (or -c-ok (.-cok context))
            (or -e-ok (.-eok context))
            (or -c-err (.-cerr context))
            (or -e-err (.-eerr context))))

(defmacro update
  [context m]
  (assert (map? m))
  (assert (every? #{::c-ok ::e-ok ::c-err ::e-err} (keys m)))
  `(update-context ~context ~(::c-ok m) ~(::e-ok m) ~(::c-err m) ~(::e-err m)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defrecord Result [value consumed state error])

(defrecord Failure [consumed error])

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn new-context
  []
  (Context. (fn consumed-ok, [x s e] (Result. x true s e))
            (fn empty-ok,,,, [x s e] (Result. x false s e))
            (fn consumed-err [e],,,, (Failure. true e))
            (fn empty-err,,, [e],,,, (Failure. false e))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
