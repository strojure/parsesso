(ns strojure.parsesso.text
  (:require [clojure.string :as string]
            #?(:cljs [goog.string :as gstring])
            [strojure.parsesso.core :as p]
            [strojure.parsesso.impl.core :as impl]
            [strojure.parsesso.impl.pos :as pos]))

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
  (-> (satisfy (partial string/index-of cs))
      (p/expecting (delay (describe 'char-of cs)))))

(defn char-of-not
  "This parser succeeds if the current character /not/ in the supplied list of
  characters. Returns the parsed character."
  [cs]
  (-> (satisfy (complement (partial string/index-of cs)))
      (p/expecting (delay (describe 'char-of-not cs)))))

(def ^{:arglists '([c])}
  letter?
  #?(:clj #(Character/isLetter ^char %) :cljs gstring/isAlpha))

(def letter
  (-> (satisfy letter?)
      (p/expecting 'letter)))

#_#?(:clj (defn cons-str
            ([] (StringBuffer.))
            ([sb] (str sb))
            ([x sb] (-> ^StringBuffer sb (.insert 0 x)))))

;; TODO: Better name for `string`?
(defn string
  "This parser parses a sequence of characters given by `s`. Returns the parsed
  string."
  [s]
  (if-let [cs (seq s)]
    (p/>> (->> cs (map (fn [c]
                         (-> (satisfy (partial = c))
                             (p/expecting (delay (str (char-str c) " in "
                                                      (describe 'string s)))))))
               (reduce p/>>))
          (p/result s))
    (p/result s)))

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
  (toString [_] (str "line " line ", column " col)))

(defn parse
  [p input]
  ;; TODO: Initialize source pos and input seq
  (p (impl/->State (or (seq input) ())
                   (TextPos. 8 1 1)
                   nil)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
