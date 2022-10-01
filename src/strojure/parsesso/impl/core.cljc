(ns strojure.parsesso.impl.core
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

(defrecord State [input pos user])

(defn new-state
  [^State state, new-input, new-pos]
  (State. new-input new-pos (.-user state)))

(defn input
  [state]
  (.-input ^State state))

(defn pos
  [state]
  (.-pos ^State state))

(defn user
  [state]
  (.-user ^State state))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn e-ok-throw-empty-input
  [_ _ _]
  (throw (ex-info (str "Combinator is applied to a parser that accepts an empty input.") {})))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
