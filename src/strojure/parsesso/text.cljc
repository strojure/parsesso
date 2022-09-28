(ns strojure.parsesso.text
  (:refer-clojure :exclude [newline])
  (:require [clojure.string :as string]
            #?(:cljs [goog.string :as gstring])
            [strojure.parsesso.core :as p]
            [strojure.parsesso.impl.core :as impl]
            [strojure.parsesso.impl.pos :as pos])
  #?(:clj  (:import (org.apache.commons.lang3 CharUtils))
     :cljs (:import [goog.string StringBuffer])))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn- char-str [c] (pr-str (str c)))

(defn- describe
  [sym args]
  (str "(" sym " " (pr-str args) ")"))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(def ^{:doc "This parser succeeds for any character for which the supplied predicate
            function returns `true`. Returns the character that is actually parsed."
       :arglists '([pred])}
  satisfy
  (p/token-fn {:msg-fn char-str}))

(def any-char
  "This parser succeeds for any character. Returns the parsed character."
  (satisfy any?))

(defn char-of
  "This parser succeeds if the current character is in the supplied list of
  characters. Returns the parsed character. See also `satisfy`."
  [cs]
  ;; TODO: Use #(StringUtils/contains "abc" (int %)) ?
  (-> (satisfy (partial string/index-of cs))
      (p/expecting (delay (if (second cs) (describe 'char-of cs)
                                          (char-str cs))))))

(defn char-of-not
  "This parser succeeds if the current character /not/ in the supplied list of
  characters. Returns the parsed character."
  [cs]
  (-> (satisfy (complement (partial string/index-of cs)))
      (p/expecting (delay (describe 'char-of-not cs)))))

(defn matches
  "Parses a character matching regex pattern `re`. Returns the parsed character."
  ([re] (matches re (delay (describe 'matches re))))
  ([re expecting]
   (-> (satisfy #(re-matches re (str %)))
       (p/expecting expecting))))

;; TODO: Move predicate to separate namespace?
(def whitespace?
  #?(:clj  #(Character/isWhitespace ^char %)
     :cljs gstring/isBreakingWhitespace))

(def whitespace
  "Parses a whitespace character. Returns the parsed character."
  (-> (satisfy whitespace?)
      (p/expecting "whitespace character")))

(def skip-space*
  "This parser skips /zero/ or more white space characters."
  (p/skip* (satisfy whitespace?)))

(def skip-space+
  "This parser skips /one/ or more white space characters."
  (p/after whitespace skip-space*))

(def newline
  "Parses a CRLF or LF end of line. Returns a `\newline` character."
  (p/choice (char-of "\n")
            (p/after (char-of "\r") (char-of "\n"))))

(def alpha
  "Parses ASCII 7 bit alphabetic characters. Returns the parsed character."
  (-> (satisfy #?(:clj  #(CharUtils/isAsciiAlpha ^char %)
                  :cljs gstring/isAlpha))
      (p/expecting "alphabetic character")))

(def upper
  "Parses ASCII 7 bit alphabetic upper case character. Returns the parsed
  character."
  (-> (satisfy #?(:clj  #(CharUtils/isAsciiAlphaUpper ^char %)
                  :cljs (partial re-matches #"[A-Z]")))
      (p/expecting "upper case character")))

(def lower
  "Parses ASCII 7 bit alphabetic lower case character. Returns the parsed
  character."
  (-> (satisfy #?(:clj  #(CharUtils/isAsciiAlphaLower ^char %)
                  :cljs (partial re-matches #"[a-z]")))
      (p/expecting "lower case character")))

(def numeric
  "Parses ASCII 7 bit numeric character. Returns the parsed character."
  (-> (satisfy #?(:clj  #(CharUtils/isAsciiNumeric ^char %)
                  :cljs gstring/isNumeric))
      (p/expecting "numeric character")))

(def alpha-num
  "Parses ASCII 7 bit alphabetic or numeric characters. Returns the parsed
  character."
  (-> (satisfy #?(:clj  #(CharUtils/isAsciiAlphanumeric ^char %)
                  :cljs gstring/isAlphaNumeric))
      (p/expecting "alphanumeric character")))

(defn substr
  "Parses a sequence of characters given by `s`. Returns `s`."
  [s]
  (if-let [cs (seq s)]
    (p/after (->> cs (map (fn [c]
                            (-> (satisfy (partial = c))
                                (p/expecting (delay (str (char-str c) " in "
                                                         (describe 'substr s)))))))
                  (reduce p/after))
             (p/result s))
    (p/result s)))

;; TODO: Move build-string to impl namespace?
(defn build-string
  "Builds string from (possibly nested) collections of parsed characters and
  strings."
  ([x] (-> #?(:clj (StringBuilder.) :cljs (StringBuffer.))
           (build-string x)
           (str)))
  ([sb x]
   (if (sequential? x)
     (reduce build-string sb x)
     #?(:clj  (.append ^StringBuilder sb (str x))
        :cljs (.append ^StringBuffer sb (str x))))))

(defn to-str
  "Converts parser result to string."
  [p]
  (p/map-result p build-string))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn- compare*
  [x y]
  (let [c (compare x y)]
    (when-not (zero? c)
      c)))

(defrecord TextPos [tab, ^long line, ^long col]
  pos/ISourcePos
  (next-pos [pos c _input]
    (case c \tab
            (update pos :col #(-> % (+ tab) (- (mod (dec %) tab))))
            \newline
            (TextPos. tab (unchecked-inc line) 1)
            ;; default
            (TextPos. tab line (unchecked-inc col))))
  #?@(:clj
      [Comparable
       (compareTo [_ pos] (or (compare* line (:line pos))
                              (compare* col (:col pos))
                              0))]
      :cljs
      [IComparable
       (-compare [_ pos] (or (compare* line (:line pos))
                             (compare* col (:col pos))
                             0))])
  Object
  (toString [_] (str "line " line ", column " col)))

(defn parse
  [p input]
  ;; TODO: Initialize source pos and input seq
  (p (impl/->State (or (seq input) ())
                   (TextPos. 8 1 1)
                   nil)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
