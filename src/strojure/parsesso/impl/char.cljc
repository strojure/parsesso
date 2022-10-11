(ns strojure.parsesso.impl.char
  (:require [clojure.string :as string])
  #?(:cljs (:import [goog.string StringBuffer])))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn equals-ignorecase
  "True if chars are equal, case insensitive. "
  [c1 c2]
  (or (= c1 c2) #?(:clj  (.equals ^Object (Character/toLowerCase ^char c1)
                                  (Character/toLowerCase ^char c2))
                   :cljs (= (string/lower-case c1)
                            (string/lower-case c2)))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(def ^:private one-of-pred-fn!
  (atom {}))

(defn register-one-of-pred-fn
  "Associates keyword `k` with predicate function of the `one-of?` and `not-of?`
  parsers."
  [k, f]
  (assert (keyword k) "Requires keyword as `one-of?` test-fn ID")
  (swap! one-of-pred-fn! assoc k f))

(defn one-of-pred-fn
  "Returns predicate for the keyword `k` and string of characters `cs`."
  [k cs]
  (if-let [f (@one-of-pred-fn! k)]
    (f cs)
    (throw (ex-info (str "The `one-of?` predicate function is not registered:" k) {}))))

(defn one-of-pred-default
  "Default predicate for `one-of?` and `not-of?` parsers."
  [cs]
  (fn [c] #?(:clj
             (<= 0 (.indexOf ^String cs ^int (.charValue ^Character c)))
             :cljs
             (string/index-of cs c))))

(defn one-of-pred-ignorecase
  "Default predicate for `one-of?` and `not-of?` parsers."
  [cs]
  (let [cs (string/lower-case cs)]
    (fn [c] #?(:clj
               (<= 0 (.indexOf ^String cs ^int (Character/toLowerCase ^char c)))
               :cljs
               (string/index-of cs (string/lower-case c))))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn deep-join
  "Builds string from (possibly nested) collections of parsed characters and
  strings."
  ([x] (-> #?(:clj (StringBuilder.) :cljs (StringBuffer.))
           (deep-join x)
           (str)))
  ([sb x]
   (if (sequential? x)
     (reduce deep-join sb x)
     #?(:clj  (.append ^StringBuilder sb (str x))
        :cljs (.append ^StringBuffer sb (str x))))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
