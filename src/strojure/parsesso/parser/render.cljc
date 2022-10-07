(ns strojure.parsesso.parser.render)

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defprotocol IRender
  (render [obj]))

#?(:clj
   (extend-protocol IRender
     nil
     (render [x] (pr-str x))
     Object
     (render [x] (pr-str x))
     Character
     (render [c] (pr-str (str c))))

   :cljs
   (extend-protocol IRender
     nil
     (render [x] (pr-str x))
     object
     (render [x] (pr-str x))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
