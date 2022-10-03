(ns strojure.parsesso.impl.text
  (:require #?(:cljs [clojure.string :as string])
            [strojure.parsesso.impl.pos :as pos])
  #?(:cljs (:import [goog.string StringBuffer])))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn one-of?
  "True if the character `c` is in the supplied string of characters `cs`.
  Returns predicate function if called with 1 argument."
  ([cs]
   #(one-of? cs %))
  ([cs c]
   #?(:clj
      (<= 0 (.indexOf ^String cs ^int (.charValue ^Character c)))
      :cljs
      (string/index-of cs c))))

(defn ascii-white?
  "True if the character is ASCII 7 bit whitespace."
  [c]
  #?(:clj
     (Character/isSpace ^char c)
     :cljs
     (string/index-of " \n\r\t\f" c)))

(defn ascii-upper?
  "True if the character is ASCII 7 bit alphabetic upper case."
  [c]
  #?(:clj
     (let [c (unchecked-int (.charValue ^Character c))]
       (and (<= 65 c) (<= c 90)))
     :cljs
     (re-find #"[A-Z]" c)))

(defn ascii-lower?
  "True if the character is ASCII 7 bit alphabetic lower case."
  [c]
  #?(:clj
     (let [c (unchecked-int (.charValue ^Character c))]
       (and (<= 97 c) (<= c 122)))
     :cljs
     (re-find #"[a-z]" c)))

(defn ascii-alpha?
  "True if the character is ASCII 7 bit alphabetic."
  [c]
  #?(:clj
     (or (ascii-upper? c) (ascii-lower? c))
     :cljs
     (re-find #"[a-zA-Z]" c)))

(defn ascii-numeric?
  "True if the character is ASCII 7 bit numeric."
  [c]
  #?(:clj
     (let [c (unchecked-int (.charValue ^Character c))]
       (and (<= 48 c) (<= c 57)))
     :cljs
     (re-find #"[0-9]" c)))

(defn ascii-alphanumeric?
  "True if the character is ASCII 7 bit alphabetic or numeric."
  [c]
  #?(:clj
     (or (ascii-alpha? c) (ascii-numeric? c))
     :cljs
     (re-find #"[a-zA-Z0-9]" c)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn join-chars
  "Builds string from (possibly nested) collections of parsed characters and
  strings."
  ([x] (-> #?(:clj (StringBuilder.) :cljs (StringBuffer.))
           (join-chars x)
           (str)))
  ([sb x]
   (if (sequential? x)
     (reduce join-chars sb x)
     #?(:clj  (.append ^StringBuilder sb (str x))
        :cljs (.append ^StringBuffer sb (str x))))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn- compare*
  [x y]
  (let [c (compare x y)]
    (when-not (zero? c)
      c)))

(defrecord TextPos [tab, ^long line, ^long col]
  pos/InputPos
  (next-pos [pos c]
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

(defn new-text-pos
  []
  (TextPos. 8 1 1))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
