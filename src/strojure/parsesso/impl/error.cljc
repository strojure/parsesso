(ns strojure.parsesso.impl.error
  (:refer-clojure :exclude [empty?])
  (:require [clojure.string :as string]))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(declare explain-str)

(defrecord ParseError [pos messages]
  Object
  (toString [_]
    (str pos ":\n" (explain-str messages))))

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

(defn- comma-sep
  [-or xs]
  (let [xs (filter seq xs)]
    (case (count xs)
      0 ""
      1 (str (first xs))
      (str (string/join ", " (butlast xs)) " " -or " " (last xs)))))

(defn- show-many
  [xs -or prefix]
  (when xs
    (cond->> (->> xs (map (comp force second))
                  (comma-sep -or))
      prefix (str prefix " "))))

(defn explain-str
  "The standard function for showing error messages. Formats a list of error
  messages in English. The resulting string will be formatted like:

  |unexpected /{The first UnExpect or a SysUnExpect message}/;
  expecting /{comma separated list of Expect messages}/;
  /{comma separated list of Message messages}/"
  {:arglists '([{:keys [unknown expecting unexpected end-of-input or] :as dict}, messages]
               [messages])}
  ([messages] (explain-str nil messages))
  ([dict messages]
   (let [dict (->> dict (merge {:unknown "unknown parse error"
                                :expecting "expecting"
                                :unexpected "unexpected"
                                :end-of-input "end of input"
                                :or "or"}))]
     (if messages
       (let [xs (->> messages
                     (map #(update % 1 force))
                     (distinct)
                     (group-by first))]
         (->> [(when-let [[[_ msg]] (and (not (xs ::un-expect))
                                         (xs ::sys-unexpect))]
                 (str (dict :unexpected) " " (or (not-empty (force msg))
                                                 (dict :end-of-input))))
               (show-many (xs ::un-expect) (dict :or) (dict :unexpected))
               (show-many (xs ::expect) (dict :or) (dict :expecting))
               (show-many (xs ::message) (dict :or) nil)]
              (filter some?)
              (string/join "\n")))
       (dict :unknown)))))

(comment
  (->> [[::sys-unexpect nil]
        [::sys-unexpect "SysUnExpect"]
        [::un-expect "UnExpect1"]
        [::un-expect (delay "UnExpect2")]
        [::expect "Expect1"]
        [::expect (delay "Expect2")]
        [::expect "Expect2"]
        [::expect ""]
        [::expect "Expect3"]
        [::message "Message1"]
        [::message (delay "Message1")]
        [::message "Message2"]]
       (explain-str)
       (println))
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
