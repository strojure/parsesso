(ns strojure.parsesso.impl.char
  {:no-doc true}
  (:require [clojure.string :as string])
  #?(:cljs (:import [goog.string StringBuffer])))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn equals-ignorecase
  "True if chars are equal, case insensitive. "
  [c1 c2]
  (or (= c1 c2)
      #?(:bb
         (= (string/lower-case c1)
            (string/lower-case c2))
         :clj
         (.equals ^Object (Character/toLowerCase ^char c1)
                  (Character/toLowerCase ^char c2))
         :default
         (= (string/lower-case c1)
            (string/lower-case c2)))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(def ^:private string-pred-fn!
  (atom {}))

(defn register-string-pred-fn
  "Associates keyword `k` with predicate function of the `is` and `is-not`
  parsers."
  [k, f]
  (assert (keyword k) "Requires keyword as `is` test-fn ID")
  (swap! string-pred-fn! assoc k f))

(defn string-pred-fn
  "Returns predicate for the keyword `k` and string of characters `s`."
  [k s]
  (if-let [f (@string-pred-fn! k)]
    (f s)
    (throw (ex-info (str "The `is` predicate function is not registered:" k) {}))))

(defn string-pred-default
  "Default predicate for `is` and `is-not` parsers."
  [s]
  #?(:bb
     (fn [c] (string/index-of s c))
     :clj
     (if (char? s)
       (fn [c] (.equals ^Character s c))
       (fn [c] (<= 0 (.indexOf ^String s ^int (.charValue ^Character c)))))
     :default
     (fn [c] (string/index-of s c))))

(defn string-pred-ignorecase
  "Default predicate for `is` and `is-not` parsers."
  [s]
  (let [s (string/lower-case s)]
    (fn [c] #?(:bb
               (string/index-of s (string/lower-case c))
               :clj
               (<= 0 (.indexOf ^String s ^int (Character/toLowerCase ^char c)))
               :default
               (string/index-of s (string/lower-case c))))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn str*
  "Builds string from (possibly nested) collections of parsed characters and
  strings."
  ([x] (-> #?(:clj (StringBuilder.) :cljs (StringBuffer.))
           (str* x)
           (str)))
  ([sb x]
   (if (sequential? x)
     (reduce str* sb x)
     #?(:clj  (.append ^StringBuilder sb (str x))
        :cljs (.append ^StringBuffer sb (str x))))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
