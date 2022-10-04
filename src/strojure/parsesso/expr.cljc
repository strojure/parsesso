(ns strojure.parsesso.expr
  (:require [strojure.parsesso.core :as p]))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn chain-left-more
  ;; TODO: Code example from haskell
  "This parser parses _one_ or more occurrences of `p`, separated by `op`
  Returns a value obtained by a _left_ associative application of all functions
  returned by `op` to the values returned by `p`. This parser can for example be
  used to eliminate left recursion which typically occurs in expression
  grammars."
  [p op]
  (letfn [(more [x]
            (p/choice (p/when-let [f op, y p]
                        (more (f x y)))
                      (p/result x)))]
    (p/when-let [x p]
      (more x))))

(defn chain-left-zero
  "This parser parses _zero_ or more occurrences of `p`, separated by `op`.
  Returns a value obtained by a _left_ associative application of all functions
  returned by `op` to the values returned by `p`. If there are zero occurrences
  of `p`, the value `x` is returned."
  [p op x]
  (p/optional (chain-left-more p op) x))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn chain-right-more
  "This parser parses _one_ or more occurrences of `p`, separated by `op`.
  Returns a value obtained by a _right_ associative application of all functions
  returned by `op` to the values returned by `p`."
  [p op]
  (letfn [(scan []
            (p/when-let [x p]
              (more x)))
          (more [x]
            (p/choice (p/when-let [f op, y (scan)]
                        (p/result (f x y)))
                      (p/result x)))]
    (scan)))

(defn chain-right-zero
  "Parses _zero_ or more occurrences of `p`, separated by `op`. Returns a value
  obtained by a _right_ associative application of all functions returned by
  `op` to the values returned by `p`. If there are no occurrences of `p`, the
  value `x` is returned."
  [p op x]
  (p/optional (chain-right-more p op) x))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
