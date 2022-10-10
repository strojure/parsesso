(ns strojure.parsesso.char.ignore-case
  "Case insensitive char functions."
  (:require [clojure.string :as string]
            [strojure.parsesso.core :as p]))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn one-of?
  "Returns parser and predicate for the character `c` which is in the supplied
  string of characters `cs`, case insensitive."
  [cs]
  (let [cs (string/lower-case cs)]
    (p/token (fn [c] #?(:clj
                        (<= 0 (.indexOf ^String cs ^int (Character/toLowerCase ^char c)))
                        :cljs
                        (string/index-of cs (string/lower-case c))))
             (delay (if (second cs) (str "character of " (p/render cs))
                                    (p/render cs))))))

(defn not-of?
  "Returns parser and predicate for the character `c` which is _not_ in the
  supplied string of characters `cs`, case insensitive."
  [cs]
  (let [cs (string/lower-case cs)]
    (p/token (complement (one-of? cs))
             (delay (if (second cs)
                      (str "character not of " (p/render cs))
                      (str "not " (p/render cs) " character"))))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn equals
  "True if chars are equal, case insensitive. "
  [c1 c2]
  (or (= c1 c2) #?(:clj  (.equals ^Object (Character/toLowerCase ^char c1)
                                  (Character/toLowerCase ^char c2))
                   :cljs (= (string/lower-case c1)
                            (string/lower-case c2)))))

(defn word
  "Parses a sequence of chars given by `cs` and returns `cs`. Case insensitive."
  {:inline (fn [cs] `(p/word ~cs equals))}
  [cs]
  (p/word cs equals))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
