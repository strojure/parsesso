(ns strojure.parsesso.impl.error
  (:require [clojure.string :as string]
            [strojure.parsesso.impl.state :as state]))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(declare render-messages)

(defrecord ParseError [pos messages]
  Object
  (toString [_]
    (str "at " pos ":\n" (render-messages messages))))

(defn- new-error
  [state typ msg]
  (ParseError. (state/pos state) (cons [typ msg] nil)))

(defn sys-unexpected
  ([state]
   (new-error state ::sys-unexpected nil))
  ([state msg]
   (new-error state ::sys-unexpected msg)))

(defn unexpected
  [state msg]
  (new-error state ::unexpected msg))

(defn with-expecting
  [^ParseError err, msg]
  (ParseError. (.-pos err) (cons [::expecting msg] (.-messages err))))

(defn message
  [state msg]
  (new-error state ::message msg))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn merge-errors [e1 e2]
  (let [m1 (:messages e1), m2 (:messages e2)]
    (cond (and m1 (nil? m2)) e1
          (and m2 (nil? m1)) e2
          :else (let [pos1 (:pos e1)]
                  (case (compare pos1 (:pos e2))
                    1 e1, -1 e2, (ParseError. pos1 (reduce conj m1 m2)))))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn- comma-sep
  [-or xs]
  (let [xs (->> xs (map str) (filter seq))]
    (case (count xs)
      0 ""
      1 (str (first xs))
      (str (string/join ", " (butlast xs)) " " -or " " (last xs)))))

(defn- render-many
  [xs -or prefix]
  (when xs
    (cond->> (->> xs (map (comp force second))
                  (comma-sep -or))
      prefix (str prefix " "))))

(defn render-messages
  "The standard function for showing error messages. Formats a list of error
  messages in English. The resulting string will be formatted like:

  |unexpected /{The first UnExpect or a SysUnExpect message}/;
  expecting /{comma separated list of Expect messages}/;
  /{comma separated list of Message messages}/"
  {:arglists '([{:keys [unknown expecting unexpected end-of-input or] :as dict}, messages]
               [messages])}
  ([messages] (render-messages nil messages))
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
                     (reverse)
                     (group-by first))]
         (->> [(when-let [[[_ msg]] (and (not (xs ::unexpected))
                                         (xs ::sys-unexpected))]
                 ;; TODO: explicit detection of end of input?
                 (str (dict :unexpected) " " (or (not-empty (force msg))
                                                 (dict :end-of-input))))
               (render-many (xs ::unexpected) (dict :or) (dict :unexpected))
               (render-many (xs ::expecting) (dict :or) (dict :expecting))
               (render-many (xs ::message) (dict :or) nil)]
              (filter some?)
              (string/join "\n")))
       (dict :unknown)))))

(comment
  (->> [[::sys-unexpected nil]
        [::sys-unexpected "SysUnExpect"]
        [::unexpected "UnExpect1"]
        [::unexpected (delay "UnExpect2")]
        [::expecting "Expect1"]
        [::expecting (delay "Expect2")]
        [::expecting "Expect2"]
        [::expecting ""]
        [::expecting "Expect3"]
        [::message "Message1"]
        [::message (delay "Message1")]
        [::message "Message2"]]
       (render-messages)
       (println))
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
