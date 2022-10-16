(ns strojure.parsesso.expr
  "Parser combinators for expressions."
  (:require [strojure.parsesso.parser :as p]))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn +chain-left
  "Parses _one_ or more occurrences of `p`, separated by `op`. Returns a value
  obtained by a _left_ associative application of all functions returned by `op`
  to the values returned by `p`. This parser can for example be used to
  eliminate left recursion which typically occurs in expression grammars.

      (def mulop  (p/choice (p/after (char/is \\*) (p/result *))
                            (p/after (char/is \\/) (p/result /))))

      (def addop  (p/choice (p/after (char/is \\+) (p/result +))
                            (p/after (char/is \\-) (p/result -))))

      (def expr   (+chain-left term addop))
      (def term   (+chain-left factor mulop))
      (def factor (p/choice (parens expr) integer))
  "
  [p op]
  (letfn [(more [x]
            (p/choice (p/for [f op, y p]
                        (more (f x y)))
                      (p/result x)))]
    (p/for [x p]
      (more x))))

(defn *chain-left
  "Parses _zero_ or more occurrences of `p`, separated by `op`. Returns a value
  obtained by a _left_ associative application of all functions returned by `op`
  to the values returned by `p`. If there are zero occurrences of `p`, the value
  `x` is returned."
  [p op x]
  (p/option (+chain-left p op) x))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn +chain-right
  "Parses _one_ or more occurrences of `p`, separated by `op`. Returns a value
  obtained by a _right_ associative application of all functions returned by
  `op` to the values returned by `p`."
  [p op]
  (letfn [(scan []
            (p/for [x p]
              (more x)))
          (more [x]
            (p/choice (p/for [f op, y (scan)]
                        (p/result (f x y)))
                      (p/result x)))]
    (scan)))

(defn *chain-right
  "Parses _zero_ or more occurrences of `p`, separated by `op`. Returns a value
  obtained by a _right_ associative application of all functions returned by
  `op` to the values returned by `p`. If there are no occurrences of `p`, the
  value `x` is returned."
  [p op x]
  (p/option (+chain-right p op) x))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
