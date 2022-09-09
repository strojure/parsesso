(ns strojure.parsessor.core
  (:require [strojure.parsessor.impl.state :as state])
  #?(:clj (:import (clojure.lang Fn))))

#?(:clj  (set! *warn-on-reflection* true) 
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defprotocol IParser
  (parse* [this state])
  (parser? [this]))

(defrecord Parser [f]
  IParser
  (parser? [_] true)
  (parse* [_ state] (f state)))

(defn parse-pred [pred state]
  (if-let [input (seq (state/input state))]
    (let [c (first input)]
      (if (pred c)
        (state/increment state c)
        (state/->Failure pred state ::unexpected "Unexpected")))
    (state/->Failure pred state :eof "End of input")))

#?(:clj (extend-protocol IParser
          Fn
          (parser? [_] true)
          (parse* [pred state] (parse-pred pred state))
          Object
          (parser? [_] false)
          nil
          (parser? [_] false))
   :cljs (extend-protocol IParser
           function
           (parser? [_] true)
           (parse* [pred state] (parse-pred pred state))
           default
           (parser? [_] false) ))

(defn parse-state*
  [p state]
  (loop [state state, res (parse* p state)]
    (if (parser? res)
      (recur state (parse* res state))
      res)))

(defn many
  "Parses p zero or more times; returns the result(s) in a vector. It stops when
  p fails, but this parser succeeds."
  [p]
  (Parser. (fn [state]
             (loop [res (parse-state* p state) vs (transient [])]
               (if (state/state? res)
                 (recur (parse-state* p res) (conj! vs (:-value res)))
                 (state/set-value (:state res) (persistent! vs)))))))

(defn parse
  [p input]
  (parse-state* p (state/->State (seq input) nil nil)))

(comment
  (def -input (seq "a"))
  (def -input (seq "abc123"))
  (def -input (seq "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))
  (def -input (seq "123"))
  (parser? letter?)
  (defn letter? [c] (Character/isLetter ^Character c))
  (defn letter? [c] (Character/isLetter ^Character c))
  (defn letter? [c] (= "a" c))
  (parse letter? -input)
  (parse (many letter?) -input)
  (seq -input)
  (parse* letter? (state/->State -input nil nil))
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
