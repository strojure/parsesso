(ns strojure.parsesso.impl.parser
  (:require [strojure.parsesso.impl.reply :as r])
  #?(:clj (:import (clojure.lang IFn))))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

#?(:clj
   (deftype Continue [f] IFn (invoke [_] (f)))
   :cljs
   (deftype Continue [f] IFn (-invoke [_] (f))))

(defn- run
  "Executes continuation loop."
  [f state]
  (loop [ret (f state (r/new-context))]
    (if (instance? Continue ret)
      (recur (ret))
      ret)))

#?(:clj
   (deftype Parser [f]
     IFn
     (invoke [_p state] (run f state))
     (invoke [_p state context] (Continue. (fn [] (f state context)))))
   :cljs
   (deftype Parser [f]
     IFn
     (-invoke [_p state] (run f state))
     (-invoke [_p state context] (Continue. (fn [] (f state context))))))

(defn parser?
  [p]
  (instance? Parser p))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn e-ok-throw-empty-input
  [_ _]
  (throw (ex-info (str "Combinator is applied to a parser that accepts an empty input.") {})))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
