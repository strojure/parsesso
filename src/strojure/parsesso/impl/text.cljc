(ns strojure.parsesso.impl.text
  (:require #?(:cljs [goog.string :as gstring])
            [strojure.parsesso.impl.pos :as pos])
  #?(:cljs (:import [goog.string StringBuffer])))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(def whitespace?
  #?(:clj  #(Character/isWhitespace ^char %)
     :cljs gstring/isBreakingWhitespace))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

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

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
