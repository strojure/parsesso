(ns strojure.parsesso.char.core
  (:refer-clojure :exclude [newline])
  (:require #?(:cljs [clojure.string :as string])
            [strojure.parsesso.core :as p]
            [strojure.parsesso.impl.char :as impl]))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn register-one-of-pred
  "Associates keyword `k` with predicate function of the `one-of?` and `not-of?`
  parser."
  [k, f]
  (impl/register-one-of-pred-fn k f))

(register-one-of-pred :default impl/one-of-pred-default)
(register-one-of-pred :i impl/one-of-pred-ignorecase)

(defn one-of?
  "Returns parser and predicate for the character `c` which is in the supplied
  string of characters `cs`. Optional `pred-k` keyword refers to function
  `(fn [pred-k cs] (fn [c] ...))` which returns custom predicate for chars
  against `cs`. The new `pred-k` should be registered using
  `register-one-of-pred`, predefined values are `:default` for default and `:i`
  for case insensitive matching.

      (def control-char (one-of? \"EX\"))

      (def control-char-ignorecase (one-of? \"ex\" :i))
  "
  ([cs]
   (p/token (impl/one-of-pred-default cs)
            (delay (if (second cs) (str "character of " (p/render cs))
                                   (p/render cs)))))
  ([cs, pred-k]
   (p/token (impl/one-of-pred-fn pred-k cs)
            (delay (if (second cs) (str "character of " (p/render cs))
                                   (p/render cs))))))

(defn not-of?
  "Returns parser and predicate for the character `c` which is _not_ in the
  supplied string of characters `cs`. See also `one-of?` about optional
  `pred-k` argument."
  ([cs]
   (p/token (complement (one-of? cs))
            (delay (if (second cs)
                     (str "character not of " (p/render cs))
                     (str "not " (p/render cs) " character")))))
  ([cs, pred-k]
   (p/token (complement (one-of? cs pred-k))
            (delay (if (second cs)
                     (str "character not of " (p/render cs))
                     (str "not " (p/render cs) " character"))))))

(defn re-match?
  "Returns parser and predicate for the character `c` matching regex pattern
  `re`."
  [re]
  (p/token (fn [c] (re-find re (str c)))
           (delay (str "character matching pattern " (p/render re)))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(def upper-case?
  "Parser and predicate for ASCII 7 bit upper-case alphabetic character."
  (p/token (fn [c] #?(:clj
                      (let [c (unchecked-int (.charValue ^Character c))]
                        (and (<= 65 c) (<= c 90)))
                      :cljs
                      (re-find #"[A-Z]" c)))
           "upper-case alphabetic character"))

(def lower-case?
  "Parser and predicate for ASCII 7 bit lower-case alphabetic character."
  (p/token (fn [c] #?(:clj
                      (let [c (unchecked-int (.charValue ^Character c))]
                        (and (<= 97 c) (<= c 122)))
                      :cljs
                      (re-find #"[a-z]" c)))
           "lower-case alphabetic character"))

(def alpha?
  "Parser and predicate for ASCII 7 bit alphabetic character."
  (p/token (fn [c] #?(:clj
                      (or (upper-case? c) (lower-case? c))
                      :cljs
                      (re-find #"[a-zA-Z]" c)))
           "alphabetic character"))

(def numeric?
  "Parser and predicate for ASCII 7 bit numeric character."
  (p/token (fn [c] #?(:clj
                      (let [c (unchecked-int (.charValue ^Character c))]
                        (and (<= 48 c) (<= c 57)))
                      :cljs
                      (re-find #"[0-9]" c)))
           "numeric character"))

(def alpha-numeric?
  "Parser and predicate for ASCII 7 bit alphabetic or numeric character."
  (p/token (fn [c] #?(:clj
                      (or (alpha? c) (numeric? c))
                      :cljs
                      (re-find #"[a-zA-Z0-9]" c)))
           "alphanumeric character"))

(def whitespace?
  "Parser and predicate for ASCII 7 bit whitespace character."
  (p/token (fn [c] #?(:clj
                      (Character/isSpace c)
                      :cljs
                      (string/index-of " \n\r\t\f" c)))
           "whitespace character"))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(def newline
  "Parses a CRLF or LF end of line. Returns a `\newline` character."
  (p/choice (one-of? "\n")
            (p/after (one-of? "\r") (one-of? "\n"))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn ++
  "Builds string from (possibly nested) collections of parsed characters and
  strings. To be used with `p/with`."
  [x]
  (impl/deep-join x))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
