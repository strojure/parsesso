(ns strojure.parsesso.impl.pos
  {:no-doc true})

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defprotocol InputPos
  (next-pos [pos token]
    "Returns new source pos for the current token.")
  (compare-pos [pos1 pos2]
    "Comparator. Returns -1/0/1 like `compare`."))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defmulti init-pos
  "Returns initial InputPos for given options and input."
  (fn [opts _input] (:pos opts)))

(defmethod init-pos :default
  [{:keys [pos]} _]
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

(extend-protocol InputPos
  nil
  (next-pos [_ _])
  (compare-pos [_ _] 0)
  #?(:clj Number :cljs number)
  (next-pos [pos _] (inc pos))
  (compare-pos [a b] (compare a b)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defrecord IndexPos [^long i]
  InputPos
  (next-pos [_ _] (IndexPos. (unchecked-inc i)))
  (compare-pos [_ pos] (compare i (:i pos)))
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
  (compare-pos [_ pos]
    (or (compare* line (:line pos))
        (compare* col (:col pos))
        0))
  Object
  (toString [_] (str "line " line ", column " col)))

(defmethod init-pos :text
  [opts _]
  (TextPos. (or (:tab opts) 8) 1 1))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
