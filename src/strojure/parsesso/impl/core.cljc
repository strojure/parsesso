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

(defprotocol IState
  (new-state [state new-input new-pos]))

(defrecord State [input pos user]
  IState
  (new-state [_ new-input new-pos] (State. new-input new-pos user)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn e-ok-throw-empty
  [sym]
  (fn [_ _ _]
    (throw (ex-info (str "Combinator '" sym "' is applied to a parser that accepts an empty input.") {}))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
