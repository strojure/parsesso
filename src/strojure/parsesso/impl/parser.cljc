(ns strojure.parsesso.impl.parser
  (:require [strojure.parsesso.impl.reply :as r])
  #?(:clj (:import (clojure.lang IFn))))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

#?(:clj  (deftype Continue [f]
           IFn (invoke [_] (f)))

   :cljs (deftype Continue [f]
           IFn (-invoke [_] (f))))

#?(:clj  (deftype Parser [f]
           IFn (invoke [_p state context] (Continue. (fn [] (f state context)))))

   :cljs (deftype Parser [f]
           IFn (-invoke [_p state context] (Continue. (fn [] (f state context))))))

(defn parser?
  "True if `p` is instance of parser."
  [p]
  (instance? Parser p))

(defn run
  "Executes continuation loop over the parser `p`."
  [p state]
  (assert (parser? p) (str "Cannot run as parser: " (pr-str p)))
  (loop [ret (p state (r/new-context))]
    (if (instance? Continue ret)
      (recur (ret))
      ret)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn e-ok-throw-empty-input
  "Throws exception in `many` combinator."
  [_ _]
  (throw (ex-info (str "Combinator is applied to a parser that accepts an empty input.") {})))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
