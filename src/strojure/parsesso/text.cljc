(ns strojure.parsesso.text
  (:refer-clojure :exclude [char])
  (:require [clojure.string :as string]
            [strojure.parsesso.core :as p]
            [strojure.parsesso.impl.core :as impl]
            [strojure.parsesso.impl.pos :as pos]))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn- char-str [c] (pr-str (str c)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn satisfy
  "This parser succeeds for any character for which the supplied predicate
  function returns `true`. Returns the character that is actually parsed."
  [pred]
  (p/token pred char-str))

(defn char
  "This parser parses a single character `c`. Returns the parsed character."
  [c]
  (-> (satisfy (partial = c))
      (p/label (delay (char-str c)))))

(def any-char
  "This parser succeeds for any character. Returns the parsed character."
  (satisfy any?))

;; TODO: Consider adding label to one-of
(defn one-of
  "This parser succeeds if the current character is in the supplied list of
  characters. Returns the parsed character. See also `satisfy`."
  [cs]
  (satisfy (partial string/index-of cs)))

(defn none-of
  "This parser succeeds if the current character /not/ in the supplied list of
  characters. Returns the parsed character."
  [cs]
  (satisfy (complement (partial string/index-of cs))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn- compare*
  [x y]
  (let [c (compare x y)]
    (when-not (zero? c)
      c)))

(defrecord TextPos [tab line col]
  pos/ISourcePos
  (next-pos [pos c _input]
    (case c
      \newline (update pos :line inc)
      \tab (update pos :col #(-> % (+ tab) (- (mod (dec %) tab))))
      (update pos :col inc)))
  #?@(:clj  (Comparable (compareTo [_ pos] (or (compare* line (:line pos))
                                               (compare* col (:col pos))
                                               0)))
      :cljs (IComparable (-compare [_ pos] (or (compare* line (:line pos))
                                               (compare* col (:col pos))
                                               0))))
  Object
  (toString [_] (str "(line " line ", column " col ")")))

(defn parse
  [p input]
  ;; TODO: Initialize source pos and input seq
  (p (impl/->State (seq input) (TextPos. 8 1 1) nil)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
