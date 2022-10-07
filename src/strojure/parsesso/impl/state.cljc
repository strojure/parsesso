(ns strojure.parsesso.impl.state
  (:require [strojure.parsesso.parser.pos :as pos])
  #?(:clj (:import (clojure.lang ISeq))))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defrecord State [input pos user])

(defn- conform-input
  [input]
  (or (seq input) ()))

(defn init-state
  "Returns new instance of parser state."
  [input pos user]
  (State. (conform-input input) pos user))

(defn next-state
  "Returns next (incremented) instance of parser state for parsed token `tok`."
  ([^State state, tok]
   (State. (#?(:clj .more :cljs -rest) ^ISeq (.-input state))
           (pos/next-pos (.-pos state) tok)
           (.-user state)))
  ([^State state, tok, user-fn]
   (State. (#?(:clj .more :cljs -rest) ^ISeq (.-input state))
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

(defn- set-field-fn
  ([field]
   (fn [state value] (assoc state field value)))
  ([field, vf]
   (fn [state value] (assoc state field (vf value)))))

(defn- update-field-fn
  ([field]
   (fn [state f] (update state field f)))
  ([field, vf]
   (fn [state f] (update state field (comp vf f)))))

(def ^{:arglists '([state input])}
  set-input
  "Returns state with input set to `input`."
  (set-field-fn :input conform-input))

(def ^{:arglists '([state pos])}
  set-pos
  "Returns state with pos set to `pos`."
  (set-field-fn :pos))

(def ^{:arglists '([state, u])}
  set-user-state
  "Returns state with user state set to `u`."
  (set-field-fn :user))

(def ^{:arglists '([state, f])}
  update-input
  "Applies function `f` to the state input. Conforms result to sequence."
  (update-field-fn :input conform-input))

(def ^{:arglists '([state, f])}
  update-pos
  "Applies function `f` to the state pos."
  (update-field-fn :pos))

(def ^{:arglists '([state, f])}
  update-user-state
  "Applies function `f` to the user state."
  (update-field-fn :user))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
