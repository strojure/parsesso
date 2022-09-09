(ns strojure.parsessor.impl.state)

#?(:clj  (set! *warn-on-reflection* true) 
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defprotocol IState
  (input [this])
  (increment [this value])
  (set-value [this value]))

(defrecord State [-input -value -location]
  IState
  ;; TODO: increment location
  (input [_] -input)
  (increment [_ value]
    (State. (rest -input) value -location))
  (set-value [_ value]
    (State. -input value -location)))

(defrecord Failure [parser state code message])

(defn state? [x] (instance? State x))

(defn failed? [x] (instance? Failure x))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
