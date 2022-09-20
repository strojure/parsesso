(ns strojure.parsesso.impl.error
  (:refer-clojure :exclude [empty?])
  (:require [clojure.string :as string]))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;; TODO: Haskell implementation

#_(defn show-error-messages
    [-or -unknown -expecting -un-expected -eof messages]
    (if messages
      ...
      -unknown))

(defrecord ParseError [pos messages]
  Object
  (toString [_]
    (->> (for [[t msg] messages, :when msg]
           (str (name t) ": " (force msg)))
         (string/join "\n"))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn new-empty [pos] (ParseError. pos nil))

(defn empty? [error] (nil? (:messages error)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn new-message [typ msg pos] (ParseError. pos [[typ msg]]))

(defn set-message
  [err typ msg]
  ;; TODO: filter duplicates
  (update err :messages (fnil conj []) [typ msg]))

(defn merge-error [e1 e2]
  (let [m1 (:messages e1), m2 (:messages e2)]
    (cond (and m1 (nil? m2)) e1
          (and m2 (nil? m1)) e2
          :else (let [pos1 (:pos e1)]
                  (case (compare pos1 (:pos e2))
                    1 e1, -1 e2, (ParseError. pos1 (reduce conj m1 m2)))))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
