(ns strojure.parsesso.impl.parser
  (:require [strojure.parsesso.impl.reply :as r]))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(deftype Continue [f])

(defn go
  "Returns continuation for the parser `p`."
  [p state context]
  (Continue. (fn [] (p state context))))

(defn run
  "Executes parser `p` in continuation loop."
  [p state]
  (loop [ret (go p state (r/new-context))]
    (if (instance? Continue ret)
      (recur ((.-f ^Continue ret)))
      ret)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn e-ok-throw-empty-input
  "Throws exception in `many` combinator."
  [_ _]
  (throw (ex-info (str "Combinator is applied to a parser that accepts an empty input.") {})))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(def ^:private word-test-fn!
  (atom {}))

(defn register-word-test-fn
  "Associates keyword `k` with test-fn of the `word` parser."
  [k, f]
  (assert (keyword k) "Requires keyword as word test-fn ID")
  (swap! word-test-fn! assoc k f))

(defn word-test-fn
  "Returns registered test-fn for the keyword `k`."
  [k]
  (or (@word-test-fn! k)
      (throw (ex-info (str "The word test-fn is not registered:" k) {}))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
