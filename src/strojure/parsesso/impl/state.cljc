(ns strojure.parsesso.impl.state
  (:require [strojure.parsesso.parser.pos :as pos])
  #?(:clj (:import (clojure.lang ISeq))))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defrecord State [input pos user])

(defn conform-input
  [input]
  (or (seq input) ()))

(defn init-state
  [input pos user]
  (State. (conform-input input) pos user))

(defn next-state
  ([^State state, tok]
   (State. (#?(:clj .more :cljs -rest) ^ISeq (.-input state))
           (pos/next-pos (.-pos state) tok)
           (.-user state)))
  ([^State state, tok, user-fn]
   (State. (#?(:clj .more :cljs -rest) ^ISeq (.-input state))
           (pos/next-pos (.-pos state) tok)
           (user-fn (.-user state)))))

(defn set-input-pos
  [^State state, input, pos]
  (State. (conform-input input) pos (.-user state)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

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

(def ^{:doc "Returns state with input set to `input`."
       :arglists '([state input])}
  set-input
  (set-field-fn :input conform-input))

(def ^{:doc "Returns state with pos set to `pos`."
       :arglists '([state pos])}
  set-pos
  (set-field-fn :pos))

(def ^{:doc "Returns state with user state set to `u`."
       :arglists '([state, u])}
  set-user-state
  (set-field-fn :user))

(def ^{:doc "Applies function `f` to the state input. Conforms result to sequence."
       :arglists '([state, f])}
  update-input
  (update-field-fn :input conform-input))

(def ^{:doc "Applies function `f` to the state pos."
       :arglists '([state, f])}
  update-pos
  (update-field-fn :pos))

(def ^{:doc "Applies function `f` to the user state."
       :arglists '([state, f])}
  update-user-state
  (update-field-fn :user))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
