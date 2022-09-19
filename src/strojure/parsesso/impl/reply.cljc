(ns strojure.parsesso.impl.reply)

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defprotocol IContext
  (c-ok, [_ value state error] "Replies with value as consumed (consumed-ok).")
  (c-err [_ error],,,,,,,,,,,, "Fails with error as consumed (consumed-error).")
  (e-ok, [_ value state error] "Replies with value as not consumed (empty-ok).")
  (e-err [_ error],,,,,,,,,,,, "Fails with error as not consumed (empty-error).")
  (set-c-ok, [_ f] "Sets new function as `c-ok`")
  (set-c-err [_ f] "Sets new function as `c-err`")
  (set-e-ok, [_ f] "Sets new function as `e-ok`")
  (set-e-err [_ f] "Sets new function as `e-err`"))

#_:clj-kondo/ignore
(deftype Context [c-ok, c-err, e-ok, e-err]
  IContext
  (c-ok, [_ x s e] (c-ok x s e))
  (c-err [_ e],,,, (c-err e))
  (e-ok, [_ x s e] (e-ok x s e))
  (e-err [_ e],,,, (e-err e))
  (set-c-ok, [_ f] (Context. f c-err e-ok, e-err))
  (set-c-err [_ f] (Context. c-ok f e-ok e-err))
  (set-e-ok, [_ f] (Context. c-ok c-err f e-err))
  (set-e-err [_ f] (Context. c-ok c-err e-ok f)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defrecord Value [value consumed state error])

(defrecord Failure [consumed error])

(defn value? [reply] (instance? Value reply))

(defn error? [reply] (instance? Failure reply))

(def consumed? :consumed)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn new-context
  []
  (Context. (fn consumed-ok, [x s e] (Value. x true s e))
            (fn consumed-err [e],,,, (Failure. true e))
            (fn empty-ok,,,, [x s e] (Value. x false s e))
            (fn empty-err,,, [e],,,, (Failure. false e))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
