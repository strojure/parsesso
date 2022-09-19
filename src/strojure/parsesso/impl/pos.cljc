(ns strojure.parsesso.impl.pos)

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defprotocol ISourcePos
  (next-pos [pos token input]
    "Returns new source pos for the current token and the rest of the tokens
    `input`."))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

#?(:clj
   (extend-protocol ISourcePos
     nil
     (next-pos [_ _ _])
     Number
     (next-pos [pos _ _] (inc pos)))
   :cljs
   (extend-protocol ISourcePos
     nil
     (next-pos [_ _ _])
     number
     (next-pos [pos _ _] (inc pos))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
