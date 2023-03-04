(ns strojure.parsesso.impl.state
  {:no-doc true}
  (:require [strojure.parsesso.impl.pos :as pos])
  #?(:clj (:import (clojure.lang ISeq))))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defrecord State [input pos user])

(defn conform-input
  "Return non-nil input."
  [input]
  (or (seq input) ()))

(defn init-state
  "Returns new instance of parser state."
  [input pos user]
  (State. (conform-input input) pos user))

(defn next-state
  "Returns next (incremented) instance of parser state for parsed token `tok`."
  ([^State state, tok]
   (State. (#?(:bb rest :clj .more :cljs -rest :default rest) ^ISeq (.-input state))
           (pos/next-pos (.-pos state) tok)
           (.-user state)))
  ([^State state, tok, user-fn]
   (State. (#?(:bb rest :clj .more :cljs -rest :default rest) ^ISeq (.-input state))
           (pos/next-pos (.-pos state) tok)
           (user-fn (.-user state)))))

(defn set-input-pos
  "Returns instance of parser state with new values of input and pos."
  [^State state, input, pos]
  (State. (conform-input input) pos (.-user state)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn input
  "Returns parsing state input."
  [state]
  (.-input ^State state))

(defn pos
  "Returns parsing state position."
  [state]
  (.-pos ^State state))

(defn user
  "Returns user state."
  [state]
  (.-user ^State state))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
