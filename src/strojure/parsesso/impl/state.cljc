(ns strojure.parsesso.impl.state
  (:require [strojure.parsesso.impl.pos :as pos])
  #?(:clj (:import (clojure.lang ISeq))))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defrecord State [input pos user])

(defn next-state
  ([^State state, tok]
   (State. (#?(:clj .more :cljs -rest) ^ISeq (.-input state))
           (pos/next-pos (.-pos state) tok)
           (.-user state)))
  ([^State state, tok, user-fn]
   (State. (#?(:clj .more :cljs -rest) ^ISeq (.-input state))
           (pos/next-pos (.-pos state) tok)
           (user-fn (.-user state)))))

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
