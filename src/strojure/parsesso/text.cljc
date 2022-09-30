(ns strojure.parsesso.text
  (:refer-clojure :exclude [char newline])
  (:require [strojure.parsesso.core :as p]
            [strojure.parsesso.impl.text :as impl]))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn char-str [c] (pr-str (str c)))

(defn describe
  [sym args]
  (str "(" sym " " (pr-str args) ")"))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(def ^{:doc
       "This parser succeeds for any character for which the supplied predicate
       function returns `true`. Returns the character that is actually parsed.
       Accepts optional second argument for expecting error message."
       :arglists
       '([pred] [pred, message])}
  char
  (let [token (p/token-fn {:msg-fn char-str})]
    (fn
      ([pred]
       (token pred))
      ([pred, message]
       (-> (token pred)
           (p/expecting message))))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn one-of
  "This parser succeeds if the current character is in the supplied string of
  characters. Returns the parsed character. Accepts optional second argument for
  expecting error message."
  ([cs]
   (one-of cs (delay (if (second cs) (describe 'one-of cs)
                                     (char-str cs)))))
  ([cs, message]
   (char (impl/one-of? cs) message)))

(defn none-of
  "This parser succeeds if the current character /not/ in the supplied list of
  characters. Returns the parsed character. Accepts optional second argument for
  expecting error message."
  ([cs]
   (none-of cs (delay (if (second cs) (describe 'none-of cs)
                                      (str "not " (char-str cs))))))
  ([cs, message]
   (char (complement (impl/one-of? cs)) message)))

(defn match
  "Parses a character matching regex pattern `re`. Returns the parsed character.
  Accepts optional second argument for expecting error message."
  ([re]
   (match re (delay (describe 'match re))))
  ([re, message]
   (char #(re-matches re (str %)) message)))

(defn string
  "Parses a sequence of characters given by `s`. Returns `s`."
  [s]
  (if-let [cs (seq s)]
    (p/after (->> cs (map (fn [c] (char (partial = c)
                                        (delay (str (char-str c) " in "
                                                    (describe 'string s))))))
                  (reduce p/after))
             (p/result s))
    (p/result s)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(def any-char
  "This parser succeeds for any character. Returns the parsed character."
  (char any?))

(def whitespace
  "Parses a whitespace character. Returns the parsed character."
  (char impl/ascii-white?
        "whitespace character"))

(def skip-white*
  "This parser skips /zero/ or more whitespace characters."
  (p/skip* (char impl/ascii-white?)))

(def skip-white+
  "This parser skips /one/ or more whitespace characters."
  (p/after whitespace skip-white*))

(def newline
  "Parses a CRLF or LF end of line. Returns a `\newline` character."
  (p/choice (one-of "\n")
            (p/after (one-of "\r") (one-of "\n"))))

(def alpha
  "Parses ASCII 7 bit alphabetic characters. Returns the parsed character."
  (char impl/ascii-alpha?
        "alphabetic character"))

(def upper
  "Parses ASCII 7 bit alphabetic upper case character. Returns the parsed
  character."
  (char impl/ascii-upper?
        "upper case character"))

(def lower
  "Parses ASCII 7 bit alphabetic lower case character. Returns the parsed
  character."
  (char impl/ascii-lower?
        "lower case character"))

(def numeric
  "Parses ASCII 7 bit numeric character. Returns the parsed character."
  (char impl/ascii-numeric?
        "numeric character"))

(def alpha-numeric
  "Parses ASCII 7 bit alphabetic or numeric characters. Returns the parsed
  character."
  (char impl/ascii-alphanumeric?
        "alphanumeric character"))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn join-chars
  "Builds string from (possibly nested) collections of parsed characters and
  strings."
  [x]
  (impl/join-chars x))

(defn ++
  "This parser joins all characters parsed by `p` to single string."
  [p]
  (p/fmap p join-chars))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn parse
  [p input]
  ;; TODO: Initialize source pos and input seq
  (p (p/new-state input (impl/->TextPos 8 1 1) nil)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
