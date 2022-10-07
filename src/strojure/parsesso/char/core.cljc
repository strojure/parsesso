(ns strojure.parsesso.char.core
  (:refer-clojure :exclude [newline])
  (:require #?(:cljs [clojure.string :as string])
            [strojure.parsesso.core :as p]
            [strojure.parsesso.impl.char :as impl]
            [strojure.parsesso.parser.render :as render]))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn one-of?
  "Predicate function returning true if the character `c` is in the supplied
  string of characters `cs`."
  [cs]
  ^{::p/expecting (delay (if (second cs) (str "character of " (pr-str cs))
                                         (render/render cs)))}
  (fn [c]
    #?(:clj
       (<= 0 (.indexOf ^String cs ^int (.charValue ^Character c)))
       :cljs
       (string/index-of cs c))))

(defn not-of?
  "Predicate function returning true if the character `c` is _not_ in the
  supplied string of characters `cs`."
  [cs]
  (-> (complement (one-of? cs))
      (p/expecting-meta (delay (if (second cs)
                                 (str "character not of " (pr-str cs))
                                 (str "not " (render/render cs) " character"))))))

(defn re-match?
  "Predicate function returning true if the character `c` is matching regex
  pattern `re`."
  [re]
  ^{::p/expecting (delay (str "character matching pattern " (pr-str re)))}
  (fn [c]
    (re-find re (str c))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(def ^{:doc "True if the character is ASCII 7 bit alphabetic upper case."
       :arglists '([c])}
  upper?
  ^{::p/expecting "upper case character"}
  (fn [c]
    #?(:clj
       (let [c (unchecked-int (.charValue ^Character c))]
         (and (<= 65 c) (<= c 90)))
       :cljs
       (re-find #"[A-Z]" c))))

(def ^{:doc "True if the character is ASCII 7 bit alphabetic upper case."
       :arglists '([c])}
  lower?
  ^{::p/expecting "lower case character"}
  (fn [c]
    #?(:clj
       (let [c (unchecked-int (.charValue ^Character c))]
         (and (<= 97 c) (<= c 122)))
       :cljs
       (re-find #"[a-z]" c))))

(def ^{:doc "True if the character is ASCII 7 bit alphabetic."
       :arglists '([c])}
  alpha?
  ^{::p/expecting "alphabetic character"}
  (fn [c]
    #?(:clj
       (or (upper? c) (lower? c))
       :cljs
       (re-find #"[a-zA-Z]" c))))

(def ^{:doc "True if the character is ASCII 7 bit numeric."
       :arglists '([c])}
  numeric?
  ^{::p/expecting "numeric character"}
  (fn [c]
    #?(:clj
       (let [c (unchecked-int (.charValue ^Character c))]
         (and (<= 48 c) (<= c 57)))
       :cljs
       (re-find #"[0-9]" c))))

(def ^{:doc "True if the character is ASCII 7 bit alphabetic or numeric."
       :arglists '([c])}
  alpha-numeric?
  ^{::p/expecting "alphanumeric character"}
  (fn [c]
    #?(:clj
       (or (alpha? c) (numeric? c))
       :cljs
       (re-find #"[a-zA-Z0-9]" c))))

(def ^{:doc "True if the character is ASCII 7 bit whitespace."
       :arglists '([c])}
  whitespace?
  ^{::p/expecting "whitespace character"}
  (fn [c]
    #?(:clj
       (Character/isSpace c)
       :cljs
       (string/index-of " \n\r\t\f" c))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(def newline
  "Parses a CRLF or LF end of line. Returns a `\newline` character."
  (p/choice (p/token (one-of? "\n"))
            (p/after (p/token (one-of? "\r")) (p/token (one-of? "\n")))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn deep-join
  "Builds string from (possibly nested) collections of parsed characters and
  strings."
  [x]
  (impl/deep-join x))

(defn ++
  "This parser joins all characters parsed by `p` to single string."
  [p]
  (p/fmap deep-join p))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
