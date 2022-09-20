(ns strojure.parsesso.core
  (:refer-clojure :exclude [or])
  (:require [strojure.parsesso.impl.core :as impl #?@(:cljs (:refer [Continue Parser]))]
            [strojure.parsesso.impl.error :as e]
            [strojure.parsesso.impl.pos :as pos]
            [strojure.parsesso.impl.reply :as r #?@(:cljs (:refer [Failure]))])
  #?(:clj  (:import (strojure.parsesso.impl.core Continue Parser)
                    (strojure.parsesso.impl.reply Failure))
     :cljs (:require-macros [strojure.parsesso.core :refer [bind]])))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn parser
  "Wraps function `(fn [state context])` in the instance of `Parser`."
  [f]
  (impl/->Parser f))

(defn parser? [p] (instance? Parser p))

(def continue impl/continue)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;;; parsers

(defn unexpected
  "The parser `unexpected msg` always fails with an unexpected error message
  `msg` without consuming any input.

  The parsers 'error', 'label' and `unexpected` are the three parsers used to
  generate error messages. Of these, only `label` is commonly used. For an
  example of the use of `unexpected`, see the definition of `not-followed-by`."
  [msg]
  (parser
    (fn [state context]
      (r/e-err context (e/new-message ::e/un-expect msg (:pos state))))))

(defn return
  [x]
  (parser
    (fn [state context]
      (r/e-ok context x state (e/new-empty (:pos state))))))

(defn bind*
  "m - parser, f - (fn [x] parser), returns parser"
  [p f]
  (parser
    (fn [state context]
      (-> context
          ;; - if (f x) consumes, those go straight up
          ;; - if (f x) doesn't consume input, but is okay, we still return in the consumed
          ;;   continuation
          ;; - if (f x) doesn't consume input, but errors, we return the error in the
          ;;   'consumed-err' continuation
          (r/set-c-ok (fn [x s e]
                        (-> context
                            (r/set-e-ok (fn [x s ee]
                                          (r/c-ok context x s (if (e/empty? e) ee (e/merge-error e ee)))))
                            (r/set-e-err (fn [ee]
                                           (r/c-err context (if (e/empty? e) ee (e/merge-error e ee)))))
                            (continue (f x) s))))
          (r/set-e-ok (fn [x s e]
                        (if (e/empty? e)
                          (continue context (f x) s)
                          ;; - in these cases, (f x) can return as empty
                          (-> context
                              (r/set-e-ok (fn [x s ee]
                                            (r/e-ok context x s (e/merge-error e ee))))
                              (r/set-e-err (fn [ee]
                                             (r/e-err context (e/merge-error e ee))))
                              (continue (f x) s)))))
          (continue p state)))))

(defn fail
  "Always fails without consuming any input."
  [msg]
  (parser
    (fn [state context]
      (r/e-err context (e/new-message ::e/message msg (:pos state))))))

;; TODO: Remove labels from API?
(defn labels
  [p messages]
  (parser
    (fn [state context]
      (letfn [(set-expect-errors [e [msg & more :as messages]]
                (cond
                  more, (->> messages (reduce (fn [e msg] (e/add-message e ::e/expect msg)) e))
                  msg,, (e/set-message e ::e/expect msg)
                  :else (e/set-message e ::e/expect "")))]
        (-> context
            (r/set-e-ok (fn [x s e]
                          (r/e-ok context x s (cond-> e (not (e/empty? e))
                                                        (set-expect-errors messages)))))
            (r/set-e-err (fn [e]
                           (r/e-err context (set-expect-errors e messages))))
            (continue p state))))))

(defn label
  "The parser `(label msg)` behaves as parser `p`, but whenever the parser `p`
  fails /without consuming any input/, it replaces expect error messages with
  the expect error message `msg`.

  This is normally used at the end of a set alternatives where we want to return
  an error message in terms of a higher level construct rather than returning
  all possible characters. For example, if the `expr` parser from the 'try'
  example would fail, the error message is: '...: expecting expression'. Without
  the `label` combinator, the message would be like '...: expecting \"let\" or
  letter', which is less friendly."
  [p msg]
  (labels p [msg]))

(defn accept
  "Behaves like parser `p`, except that it pretends that it hasn't consumed any
  input when an error occurs.

  This combinator is used whenever arbitrary look ahead is needed. Since it
  pretends that it hasn't consumed any input when `p` fails, the `choice`
  combinator will try its second alternative even when the first parser failed
  while consuming input."
  [p]
  (parser
    (fn [state context]
      (-> context
          (r/set-c-err (partial r/e-err context))
          (continue p state)))))

(defn look-ahead
  ;; TODO: Update reference to `try`.
  "Parses `p` without consuming any input. If `p` fails and consumes some input,
  so does `look-ahead`. Combine with `try` if this is undesirable."
  [p]
  (parser
    (fn [state context]
      (let [e-ok (fn [x _ _] (r/e-ok context x state (e/new-empty (:pos state))))]
        (-> context
            (r/set-c-ok e-ok)
            (r/set-e-ok e-ok)
            (continue p state))))))

(defn token
  "Returns the parser which accepts a token when `(pred token)` returns logical
  true. The token can be shown in error message using `(msg-fn token)`."
  ([pred],,,,,,,,,,,,,,,, (token pred pos/next-pos (partial str "token: ") nil))
  ([pred, msg-fn],,,,,,,, (token pred pos/next-pos msg-fn nil))
  ;; TODO: split to two versions for get-next-user like in haskell (for performance?)
  ([pred, pos-fn, msg-fn] (token pred pos-fn msg-fn nil))
  ([pred, pos-fn, msg-fn, user-fn]
   (parser
     (fn [state context]
       (if-let [input (seq (:input state))]
         (let [tok (first input)]
           (if (pred tok)
             (let [pos (:pos state)
                   new-input (rest input)
                   new-pos (pos-fn pos tok new-input)
                   new-state (impl/->State new-input new-pos (cond->> (:user state)
                                                               user-fn (user-fn pos tok new-input)))]
               (r/c-ok context tok new-state (e/new-empty new-pos)))
             (r/e-err context (e/new-message ::e/sys-unexpect (delay (msg-fn tok)) (:pos state)))))
         (r/e-err context (e/new-message ::e/sys-unexpect "" (:pos state))))))))

(comment
  (def -input "abc")
  (parse (token #(= \a %)) -input)
  (parse (token #(= \b %)) -input)
  (parse (choice (token #(= \a %)) (token #(= \b %))) -input)
  (parse (choice (token #(= \b %)) (token #(= \a %))) -input)
  )

;; TODO: return nil or [] for empty result?
(defn many*
  "Applies the parser `p` zero or more times. Returns a vector of the returned
  values or `p`. Optional `init` is a collection to add values to."
  ([p] (many* p []))
  ([p init]
   (parser
     (fn [state context]
       (let [my-context (-> context (r/set-e-ok (impl/throw-empty-input 'many*)))
             walk (fn walk [xs x s _e]
                    (let [xs (conj! xs x)]
                      (-> my-context
                          (r/set-c-ok (partial walk xs))
                          (r/set-e-err (fn [e]
                                         (r/c-ok context (persistent! xs) s e)))
                          (continue p s))))]
         (-> my-context
             (r/set-c-ok (partial walk (transient init)))
             (r/set-e-err (partial r/e-ok context init state))
             (continue p state)))))))

(comment
  (def -input "")
  (def -input (seq "abc123"))
  (def -input (seq "123"))
  (def -input (repeat 10000 \a))
  (parse (many* (token #(Character/isLetter ^char %))) -input)
  )

(defn skip*
  "Applies the parser `p` zero or more times, skipping its result."
  [p]
  (parser
    (fn [state context]
      (let [my-context (-> context (r/set-e-ok (impl/throw-empty-input 'skip*)))
            walk (fn walk [_x s _e]
                   (-> my-context
                       (r/set-c-ok walk)
                       (r/set-e-err (partial r/c-ok context nil s))
                       (continue p s)))]
        (-> my-context
            (r/set-c-ok walk)
            (r/set-e-err (partial r/e-ok context nil state))
            (continue p state))))))

(comment
  (def -input "")
  (def -input (seq "abc123"))
  (def -input (seq "123"))
  (def -input (repeat 10000 \a))
  (parse (skip* (token #(Character/isLetter ^char %))) -input)
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;;; combinators

(defmacro bind
  [[& bindings] & body]
  ;; TODO: validate macro arguments
  (let [[sym p] (take 2 bindings)]
    (if (= 2 (count bindings))
      `(bind* ~p (fn [~sym] (let [p# ~@body]
                              ;; Allow return value directly in body
                              (cond-> p# (not (parser? p#)) (return)))))
      `(bind* ~p (fn [~sym] (bind ~(drop 2 bindings) ~@body))))))

(defn or
  "Tries to apply the parsers in order, until one of them succeeds. Returns the
  value of the succeeding parser."
  ([p1 p2]
   (parser
     (fn [state context]
       (-> context
           (r/set-e-err (fn [e]
                          (-> context
                              (r/set-e-ok (fn [x s ee]
                                            (r/e-ok context x s (e/merge-error e ee))))
                              (r/set-e-err (fn [ee]
                                             (r/e-err context (e/merge-error e ee))))
                              (continue p2 state))))
           (continue p1 state)))))
  ([p1 p2 p3]
   (-> (or p1 p2) (or p3)))
  ([p1 p2 p3 & more]
   (reduce or (list* p1 p2 p3 more))))

;; TODO: Better name for `option`?
(defn option
  "Tries to apply parser `p`. If `p` fails without consuming input, it returns
  the value `x`, otherwise the value returned by `p`."
  [p x]
  (or p (return x)))

(defn optional
  "Tries to apply parser `p`. It will parse `p` or nothing. It only fails if `p`
  fails after consuming input. It discards the result of `p`."
  [p]
  (or (bind [_ p] (return nil))
      (return nil)))

(defn between
  "Parses `open`, followed by `p` and `close`. Returns the value returned by `p`."
  ([p around] (between p around around))
  ([p open close]
   (bind [_ open, x p, _ close]
     (return x))))

(defn skip+
  "Applies the parser `p` /one/ or more times, skipping its result."
  [p]
  (bind [_ p]
    (skip* p)))

(defn many+
  "Applies the parser `p` /one/ or more times. Returns a list of the returned
  values of `p`."
  [p]
  (bind [x p]
    (many* p [x])))

;; TODO: argument order
;; TODO: Check if it should be consumed or not if n > length.
;; TODO: Rewrite similar to haskell?
(defn many-count
  "Parses `n` occurrences of `p`. If `n` is smaller or equal to zero, the parser
  equals to `(value nil)`. Returns a list of `n` values returned by `p`."
  [n p]
  (if (pos? n) (bind [x p, xs (many-count (dec n) p)]
                 (return (cons x xs)))
               (return nil)))

(comment
  (parse (many-count 2 (token #{\x})) "xxxyyy")
  (parse (many-count 4 (token #{\x})) "xxxyyy")
  )

(declare sep-by+)

(defn sep-by*
  "Parses /zero/ or more occurrences of `p`, separated by `sep`. Returns a
  vector of values returned by `p`."
  [p sep]
  (or (sep-by+ p sep)
      (return nil)))

(defn sep-by+
  "Parses /one/ or more occurrences of `p`, separated by `sep`. Returns a vector
  of values returned by `p`."
  [p sep]
  (bind [x p]
    (many* (bind [_ sep] p) [x])))

(defn sep-by-end*
  "Parses /zero/ or more occurrences of `p`, separated and ended by `sep`.
  Returns a list of values returned by `p`."
  [p sep]
  (many* (bind [x p, _ sep] (return x))))

(defn sep-by-end+
  "Parses /one/ or more occurrences of `p`, separated and ended by `sep`.
  Returns a list of values returned by `p`."
  [p sep]
  (many+ (bind [x p, _ sep] (return x))))

(declare sep-by-end?+)

(defn sep-by-end?*
  "Parses /zero/ or more occurrences of `p`, separated and optionally ended by
  `sep`. Returns a list of values returned by `p`."
  [p sep]
  (or (sep-by-end?+ p sep)
      (return nil)))

(defn sep-by-end?+
  "Parses /one/ or more occurrences of `p`, separated and optionally ended by
  `sep`. Returns a vector of values returned by `p`."
  [p sep]
  (bind [x p]
    (or (bind [_ sep, xs (sep-by-end?* p sep)]
          ;; TODO: cons?
          (return (cons x xs)))
        (return [x]))))

(declare chain-right+)

(defn chain-right*
  "Parses /zero/ or more occurrences of `p`, separated by `op`. Returns a value
  obtained by a /right/ associative application of all functions returned by
  `op` to the values returned by `p`. If there are no occurrences of `p`, the
  value `x` is returned."
  [p op x]
  (or (chain-right+ p op)
      (return x)))

(defn chain-right+
  "Parses /one/ or more occurrences of `p`, separated by `op`. Returns a value
  obtained by a /right/ associative application of all functions returned by
  `op` to the values returned by `p`."
  [p op]
  (letfn [(scan [] (bind [x p] (more x)))
          (more [x] (or (bind [f op, y (scan)] (return (f x y)))
                        (return x)))]
    (scan)))

(declare chain-left+)

(defn chain-left*
  "Parses /zero/ or more occurrences of `p`, separated by `op`. Returns a value
  obtained by a /left/ associative application of all functions returned by `op`
  to the values returned by `p`. If there are zero occurrences of `p`, the value
  `x` is returned."
  [p op x]
  (or (chain-left+ p op)
      (return x)))

(defn chain-left+
  "Parses /one/ or more occurrences of `p`, separated by `op` Returns a value
  obtained by a /left/ associative application of all functions returned by `op`
  to the values returned by `p`. This parser can for example be used to
  eliminate left recursion which typically occurs in expression grammars."
  [p op]
  (letfn [(more [x] (or (bind [f op, y p] (more (f x y)))
                        (return x)))]
    (bind [x p]
      (return (more x)))))

;;; Tricky combinators

(def any-token
  "Accepts any kind of token. It is for example used to implement 'eof'. Returns
  the accepted token."
  (token any? (fn [pos _ _] pos)))

(defn not-followed-by
  "Only succeeds when parser `p` fails. This parser does not consume any input.
  This parser can be used to implement the 'longest match' rule. For example,
  when recognizing keywords (for example `let`), we want to make sure that a
  keyword is not followed by a legal identifier character, in which case the
  keyword is actually an identifier (for example `lets`)."
  [p]
  (accept (or (bind [c (accept p)] (unexpected (delay (str c))))
              (return nil))))

(def eof
  "This parser only succeeds at the end of the input. This is not a primitive
  parser but it is defined using 'notFollowedBy'."
  ;; TODO: Implement using direct access to input for performance?
  (-> (not-followed-by any-token)
      (label "end of input")))

(defn many-till
  "Applies parser `p` /zero/ or more times until parser `end` succeeds. Returns
  the list of values returned by `p`."
  [p end]
  (letfn [(scan [] (or (bind [_ end]
                         (return nil))
                       (bind [x p, xs (scan)]
                         (return (cons x xs)))))]
    (scan)))

(defn debug-state
  "Prints the remaining parser state at the time it is invoked. It is intended
  to be used for debugging parsers by inspecting their intermediate states."
  [label]
  (or (accept (bind [x (accept (many+ any-token))
                     _ (do (println (str label ": " x))
                           (accept eof))]
                (fail x)))
      (return nil)))

(defn debug-parser
  "Prints to the console the remaining parser state at the time it is invoked.
  It then continues to apply parser `p`, and if `p` fails will indicate that the
  label has been backtracked. It is intended to be used for debugging parsers by
  inspecting their intermediate states."
  [label p]
  (bind [_ (debug-state label)]
    (or p
        (do (println (str label "  backtracked"))
            (fail label)))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn parse
  [p input]
  ;; TODO: Initialize source pos
  (loop [reply (p (impl/->State (seq input) 1 nil))]
    (if (instance? Continue reply)
      (recur (impl/run-cont reply))
      reply)))

(defn error? [reply] (instance? Failure reply))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,