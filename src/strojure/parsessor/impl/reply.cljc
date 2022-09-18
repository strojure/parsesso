(ns strojure.parsessor.impl.reply)

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defprotocol IContext
  (consumed-ok, [_ value state error])
  (consumed-err [_ error])
  (empty-ok,,,, [_ value state error])
  (empty-err,,, [_ error])
  (set-consumed-ok, [_ f])
  (set-consumed-err [_ f])
  (set-empty-ok,,,, [_ f])
  (set-empty-err,,, [_ f]))

#_:clj-kondo/ignore
(deftype Context [consumed-ok, consumed-err, empty-ok, empty-err]
  IContext
  (consumed-ok, [_ x s e] (consumed-ok x s e))
  (consumed-err [_ e],,,, (consumed-err e))
  (empty-ok,,,, [_ x s e] (empty-ok x s e))
  (empty-err,,, [_ e],,,, (empty-err e))
  (set-consumed-ok, [_ f] (Context. f, consumed-err, empty-ok, empty-err))
  (set-consumed-err [_ f] (Context. consumed-ok, f, empty-ok, empty-err))
  (set-empty-ok,,,, [_ f] (Context. consumed-ok, consumed-err, f, empty-err))
  (set-empty-err,,, [_ f] (Context. consumed-ok, consumed-err, empty-ok, f)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defprotocol IReply
  (reply [this ctx]))

(defrecord Value [value consumed state error]
  IReply
  (reply [_ ctx]
    (if consumed
      (consumed-ok ctx value state error)
      (empty-ok ctx value state error))))

(defrecord Failure [consumed error]
  IReply
  (reply [_ ctx]
    (if consumed
      (consumed-err ctx error)
      (empty-err ctx error))))

(defn value? [reply] (instance? Value reply))

(defn failure? [reply] (instance? Failure reply))

(def consumed? :consumed)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn new-context
  []
  (Context. (fn consumed-ok, [x s e] (Value. x true s e))
            (fn consumed-err [e],,,, (Failure. true e))
            (fn empty-ok,,,, [x s e] (Value. x false s e))
            (fn empty-err,,, [e],,,, (Failure. false e))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
