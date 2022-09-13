(ns strojure.parsessor.impl.reply)

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defrecord ValueReply [value consumed state error])
(defrecord ErrorReply [error consumed])

(defn value-reply? [reply] (instance? ValueReply reply))
(defn error-reply? [reply] (instance? ErrorReply reply))

(defprotocol IReplyApi
  (consumed-value [_ value state error])
  (consumed-error [_ error])
  (empty-value,,, [_ value state error])
  (empty-error,,, [_ error])
  (set-consumed-value [_ f])
  (set-consumed-error [_ f])
  (set-empty-value,,, [_ f])
  (set-empty-error,,, [_ f]))

#_:clj-kondo/ignore
(deftype ReplyApi [consumed-value, consumed-error, empty-value, empty-error]
  IReplyApi
  (consumed-value [_ value state error] (consumed-value value state error))
  (consumed-error [_ error] (consumed-error error))
  (empty-value,,, [_ value state error] (empty-value value state error))
  (empty-error,,, [_ error] (empty-error error))
  (set-consumed-value [_ f] (ReplyApi. f, consumed-error, empty-value, empty-error))
  (set-consumed-error [_ f] (ReplyApi. consumed-value, f, empty-value, empty-error))
  (set-empty-value,,, [_ f] (ReplyApi. consumed-value, consumed-error, f, empty-error))
  (set-empty-error,,, [_ f] (ReplyApi. consumed-value, consumed-error, empty-value, f)))

(defn init-api
  []
  (ReplyApi. (fn consumed-value [value state error] (ValueReply. value true state error))
             (fn consumed-error [error] (ErrorReply. error true))
             (fn empty-value,,, [value state error] (ValueReply. value false state error))
             (fn empty-error,,, [error] (ErrorReply. error false))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
