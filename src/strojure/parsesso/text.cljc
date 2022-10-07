(ns strojure.parsesso.text
  (:refer-clojure :exclude [char newline])
  (:require #?(:cljs [clojure.string :as string])
            [strojure.parsesso.impl.text :as impl]
            [strojure.parsesso.parser :as p]))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn render-char
  [c]
  (pr-str (str c)))

(def ^{:doc "This parser succeeds for any character for which the supplied predicate
  function returns `true`. Returns the character that is actually parsed.
  Accepts optional second argument for expecting error message.

      (def digit
        (char #(Character/isDigit ^char %)))
  "
       :arglists '([pred] [pred, msg])}
  char
  (p/token* {:render-token-fn render-char}))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn one-of?
  "Predicate function returning true if the character `c` is in the supplied
  string of characters `cs`."
  [cs]
  ^{::p/expecting (delay (if (second cs) (str "character of " (pr-str cs))
                                         (render-char cs)))}
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
                                 (str "not " (render-char cs) " character"))))))

(defn match?
  "Predicate function returning true if the character `c` is matching regex
  pattern `re`."
  [re]
  ^{::p/expecting (delay (str "character matching pattern " (pr-str re)))}
  (fn [c]
    (re-find re (str c))))

(def ^{:doc "Parses a sequence of characters given by `s`.
  Returns `s`.

      (def div-or-mod
        (choice (string \"div\") (string \"mod\")))
  "
       :arglists '([s])}
  string
  (p/tokens* {:render-token-fn render-char}))

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
       (Character/isSpace ^char c)
       :cljs
       (string/index-of " \n\r\t\f" c))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(def newline
  "Parses a CRLF or LF end of line. Returns a `\newline` character."
  (p/choice (char (one-of? "\n"))
            (p/after (char (one-of? "\r")) (char (one-of? "\n")))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn join-chars
  "Builds string from (possibly nested) collections of parsed characters and
  strings."
  [x]
  (impl/join-chars x))

(defn ++
  "This parser joins all characters parsed by `p` to single string."
  [p]
  (p/fmap join-chars p))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
