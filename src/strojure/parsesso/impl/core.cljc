(ns strojure.parsesso.impl.core
  (:require [strojure.parsesso.impl.reply :as r #?@(:cljs (:refer [Context]))])
  #?(:clj (:import (clojure.lang IFn)
                   (strojure.parsesso.impl.reply Context))))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defprotocol ICont
  (run-cont [c]))

(deftype Continue [f]
  ICont (run-cont [_] (f)))

(defprotocol IParser
  (continue [p state context]
    "Applies parser function in continuation."))

#?(:clj
   (deftype Parser [f]
     IFn
     (invoke [_p state] (f state (r/new-context)))
     (invoke [_p state context] (f state context))
     IParser
     (continue [_p state context] (Continue. (fn [] (f state context)))))
   :cljs
   (deftype Parser [f]
     IFn
     (-invoke [_p state] (f state (r/new-context)))
     (-invoke [_p state context] (f state context))
     IParser
     (continue [_p state context] (Continue. (fn [] (f state context))))))

(extend-type Context
  IParser
  (continue [context p state]
    ;; No reuse `(continue p state context)` for performance.
    (Continue. (fn [] (p state context)))))

(defrecord State [input pos user])

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn run
  "Executes continuation loop starting from parser `p`."
  [p state]
  (loop [reply (p state)]
    (if (instance? Continue reply)
      (recur (run-cont reply))
      reply)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn throw-empty-input
  [sym]
  (fn [_ _ _]
    (throw (ex-info (str "Combinator '" sym "' is applied to a parser that accepts an empty input.") {}))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
