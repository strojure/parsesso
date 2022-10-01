(ns strojure.parsesso.impl.pos)

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defprotocol InputPos
  (next-pos [pos token]
    "Returns new source pos for the current token."))

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

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
