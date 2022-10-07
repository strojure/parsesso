(ns strojure.parsesso.parser.pos)

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defprotocol InputPos
  (next-pos [pos token]
    "Returns new source pos for the current token."))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defmulti init-pos
  "Returns initial InputPos for given options and input."
  (fn [opts _input] (:initial-pos opts)))

(defmethod init-pos :default
  [{pos :initial-pos} _]
  (when (keyword? pos)
    (throw (ex-info (str "Cannot init input position for: " pos) {}))
    pos))

(defmethod init-pos nil
  [opts input]
  ;; Tries to detect text input and use text pos.
  (let [f (get-method init-pos (if (or (string? input) (char? (first input)))
                                 :text :sequence))]
    (f opts input)))

(defmethod init-pos :disabled
  [_ _]
  nil)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

#?(:clj
   (extend-protocol InputPos
     nil
     (next-pos [_ _])
     Number
     (next-pos [pos _] (inc pos)))
   :cljs
   (extend-protocol InputPos
     nil
     (next-pos [_ _])
     number
     (next-pos [pos _] (inc pos))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defrecord IndexPos [^long i]
  InputPos
  (next-pos [_ _] (IndexPos. (unchecked-inc i)))
  #?@(:clj  (Comparable (compareTo [_ pos] (compare i (:i pos))))
      :cljs (IComparable (-compare [_ pos] (compare i (:i pos)))))
  Object
  (toString [_] (str "index " i)))

(defmethod init-pos :sequence
  [_ _]
  (IndexPos. 0))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn- compare*
  [x y]
  (let [c (compare x y)]
    (when-not (zero? c)
      c)))

(defrecord TextPos [tab, ^long line, ^long col]
  InputPos
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

(defmethod init-pos :text
  [opts _]
  (TextPos. (or (:tab-size opts) 8) 1 1))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
