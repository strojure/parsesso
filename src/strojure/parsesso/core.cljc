(ns strojure.parsesso.core
  (:refer-clojure :exclude [sequence when-let])
  (:require [strojure.parsesso.impl.core :as impl #?@(:cljs (:refer [Parser]))]
            [strojure.parsesso.impl.error :as e]
            [strojure.parsesso.impl.pos :as pos]
            [strojure.parsesso.impl.reply :as r #?@(:cljs (:refer [Failure replace]))])
  #?(:clj  (:import (clojure.lang ISeq)
                    (strojure.parsesso.impl.core Parser)
                    (strojure.parsesso.impl.reply Failure))
     :cljs (:require-macros [strojure.parsesso.core :refer [do-parser when-let]])))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn parser
  "Wraps function `(fn [state context])` in the instance of `Parser`."
  [f]
  (impl/->Parser f))

(defn parser? [p] (instance? Parser p))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;;; parsers

(defn result
  "This parser always succeeds with value `x` without consuming any input."
  [x]
  (parser
    (fn [state context]
      (r/e-ok context x state (e/new-empty (:pos state))))))

(defn fail
  "This parser always fails with message `msg` without consuming any input."
  [msg]
  (parser
    (fn [state context]
      (r/e-err context (e/new-message ::e/message msg (:pos state))))))

(defn expecting
  "This parser behaves as parser `p`, but whenever the parser `p` fails /without
  consuming any input/, it replaces expect error messages with the expect error
  message `msg`.

  This is normally used at the end of a set alternatives where we want to return
  an error message in terms of a higher level construct rather than returning
  all possible characters. For example, if the `expr` parser from the 'silent'
  example would fail, the error message is: '...: expecting expression'. Without
  the `expecting` combinator, the message would be like '...: expecting \"let\"
  or letter', which is less friendly."
  [p msg]
  (parser
    (fn [state context]
      (letfn [(set-expect-message [e msg]
                (e/set-message e ::e/expect (or msg "")))
              (e-ok [x s e]
                (r/e-ok context x s (cond-> e (not (e/empty? e)) (set-expect-message msg))))
              (e-err [e]
                (r/e-err context (set-expect-message e msg)))]
        (p state (r/replace context {r/e-ok e-ok
                                     r/e-err e-err}))))))

(defn unexpected
  "This parser always fails with an unexpected error message `msg` without
  consuming any input.

  The parsers 'fail', 'expecting' and `unexpected` are the three parsers used to
  generate error messages. Of these, only `expecting` is commonly used. For an
  example of the use of `unexpected`, see the definition of `not-followed-by`."
  [msg]
  (parser
    (fn [state context]
      (r/e-err context (e/new-message ::e/un-expect msg (:pos state))))))

(defn silent
  "This parser behaves like parser `p`, except that it pretends that it hasn't
  consumed any input when an error occurs.

  This combinator is used whenever arbitrary look ahead is needed. Since it
  pretends that it hasn't consumed any input when `p` fails, the `choice`
  combinator will try its second alternative even when the first parser failed
  while consuming input."
  [p]
  (parser
    (fn [state context]
      (p state (r/replace context {r/c-err (partial r/e-err context)})))))

(defn look-ahead
  "This parser parses `p` without consuming any input. If `p` fails and consumes
  some input, so does `look-ahead`. Combine with `accept` if this is
  undesirable."
  [p]
  (parser
    (fn [state context]
      (letfn [(e-ok [x _ _]
                (r/e-ok context x state (e/new-empty (:pos state))))]
        (p state (r/replace context {r/c-ok e-ok
                                     r/e-ok e-ok}))))))

(defn token-fn
  "This parser accepts a token when `(pred token)` returns logical true. The
  token can be shown in error message using `(msg-fn token)`."
  [{:keys [msg-fn, user-fn] :or {msg-fn pr-str}}]
  (if-not user-fn
    (fn [pred]
      (parser
        (fn [state context]
          (if-let [input (-> ^ISeq (:input state) #?(:clj .seq :cljs -seq))]
            (let [tok (#?(:clj .first :cljs -first) input)]
              (if (pred tok)
                (let [pos (:pos state)
                      new-input (#?(:clj .more :cljs -rest) input)
                      new-pos (pos/next-pos pos tok new-input)]
                  (r/c-ok context tok (impl/new-state state new-input new-pos) (e/new-empty new-pos)))
                (r/e-err context (e/new-message ::e/sys-unexpect (delay (msg-fn tok)) (:pos state)))))
            (r/e-err context (e/new-message ::e/sys-unexpect "" (:pos state)))))))
    (fn [pred]
      (parser
        (fn [state context]
          (if-let [input (-> ^ISeq (:input state) #?(:clj .seq :cljs -seq))]
            (let [tok (#?(:clj .first :cljs -first) input)]
              (if (pred tok)
                (let [pos (:pos state)
                      new-input (#?(:clj .more :cljs -rest) input)
                      new-pos (pos/next-pos pos tok new-input)
                      new-state (impl/->State new-input new-pos (cond->> (:user state)
                                                                  user-fn (user-fn pos tok new-input)))]
                  (r/c-ok context tok new-state (e/new-empty new-pos)))
                (r/e-err context (e/new-message ::e/sys-unexpect (delay (msg-fn tok)) (:pos state)))))
            (r/e-err context (e/new-message ::e/sys-unexpect "" (:pos state)))))))))

(def token
  "This parser accepts a token when `(pred token)` returns logical true. See
  `token-fn` for customized version of the parser."
  (token-fn {}))

(def any-token
  "This parser accepts any kind of token. Returns the accepted token."
  (token any?))

(defn not-followed-by
  "This parser only succeeds when parser `p` fails. This parser does not consume
  any input. This parser can be used to implement the 'longest match' rule. For
  example, when recognizing keywords (for example `let`), we want to make sure
  that a keyword is not followed by a legal identifier character, in which case
  the keyword is actually an identifier (for example `lets`)."
  [p]
  (parser
    (fn [state context]
      (letfn [(e-ok [x _s _e]
                (r/e-err context (e/new-message ::e/un-expect (delay (pr-str x))
                                                (:pos state))))
              (e-err [_e]
                (r/e-ok context nil state (e/new-empty (:pos state))))]
        (p state (r/replace context {r/c-ok e-ok
                                     r/e-ok e-ok
                                     r/c-err e-err
                                     r/e-err e-err}))))))

(defn many*
  "This parser applies the parser `p` zero or more times. Returns a sequence of
  the returned values or `p`."
  [p]
  (parser
    (fn [state context]
      (letfn [(walk [xs x s _e]
                (let [xs (conj! xs x)]
                  (p s (r/replace context {r/c-ok (partial walk xs)
                                           r/e-ok impl/e-ok-throw-empty-input
                                           r/e-err (fn [e] (r/c-ok context (seq (persistent! xs)) s e))}))))]
        (p state (r/replace context
                            {r/c-ok (partial walk (transient []))
                             r/e-ok impl/e-ok-throw-empty-input
                             r/e-err (partial r/e-ok context nil state)}))))))

(defn skip*
  "This parser applies the parser `p` zero or more times, skipping its result."
  [p]
  (parser
    (fn [state context]
      (letfn [(walk [_x s _e]
                (p s (r/replace context {r/c-ok walk
                                         r/e-ok impl/e-ok-throw-empty-input
                                         r/e-err (partial r/c-ok context nil s)})))]
        (p state (r/replace context
                            {r/c-ok walk
                             r/e-ok impl/e-ok-throw-empty-input
                             r/e-err (partial r/e-ok context nil state)}))))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;;; combinators

(defn bind
  "m - parser, f - (fn [x] parser), returns parser"
  [p f]
  (parser
    (fn [state context]
      (letfn [(c-ok-p [x s e]
                ;; - if (f x) doesn't consume input, but is okay, we still return in the consumed
                ;; continuation
                ;; - if (f x) doesn't consume input, but errors, we return the error in the
                ;; 'consumed-err' continuation
                (letfn [(c-ok-fx [x s ee]
                          (r/c-ok context x s (if (e/empty? e) ee (e/merge-error e ee))))
                        (c-err-fx [ee]
                          (r/c-err context (if (e/empty? e) ee (e/merge-error e ee))))]
                  ((f x) s (r/replace context {r/e-ok c-ok-fx
                                               r/e-err c-err-fx}))))
              (e-ok-p [x s e]
                (if (e/empty? e)
                  ((f x) s context)
                  ;; - in these cases, (f x) can return as empty
                  (letfn [(e-ok-fx [x s ee]
                            (r/e-ok context x s (e/merge-error e ee)))
                          (e-err-fx [ee]
                            (r/e-err context (e/merge-error e ee)))]
                    ((f x) s (r/replace context {r/e-ok e-ok-fx
                                                 r/e-err e-err-fx})))))]
        (p state (r/replace context {r/c-ok c-ok-p
                                     r/e-ok e-ok-p}))))))

(defmacro do-parser
  [& body]
  (let [state (gensym) context (gensym)]
    `(parser
       (fn [~state ~context]
         ((do ~@body) ~state ~context)))))

(defmacro when-let
  [[& bindings] & body]
  (let [[sym p :as pair] (take 2 bindings)]
    (assert (= 2 (count pair)) "Requires an even number of forms in bindings")
    (assert (symbol? sym) (str "Requires symbol for binding name: " sym))
    (assert (some? body) "Requires some body")
    (if (= 2 (count bindings))
      `(bind ~p (fn [~sym] ~@body))
      `(bind ~p (fn [~sym] (when-let ~(drop 2 bindings) ~@body))))))

(defn after
  "This parser tries to apply the parsers in order, until last of them succeeds.
  Returns the value of the last parser, discards result of all preceding
  parsers."
  ([p pp]
   (bind p (fn [_] pp)))
  ([p pp ppp]
   (-> p (after pp) (after ppp)))
  ([p pp ppp & more]
   (reduce after (list* p pp ppp more))))

(defn choice
  "This parser tries to apply the parsers in order, until one of them succeeds.
  Returns the value of the succeeding parser."
  ([p pp]
   (parser
     (fn [state context]
       (letfn [(e-err-p [e]
                 (letfn [(e-ok-pp [x s ee]
                           (r/e-ok context x s (e/merge-error e ee)))
                         (e-err-pp [ee]
                           (r/e-err context (e/merge-error e ee)))]
                   (pp state (r/replace context {r/e-ok e-ok-pp
                                                 r/e-err e-err-pp}))))]
         (p state (r/replace context {r/e-err e-err-p}))))))
  ([p pp ppp]
   (-> p (choice pp) (choice ppp)))
  ([p pp ppp & more]
   (reduce choice (list* p pp ppp more))))

(defn map-result
  "This parser applies function `f` to result of the parser `p`."
  [p f]
  (when-let [x p]
    (result (f x))))

(defn sequence
  "This parser tries to apply parsers in order until all of them succeeds.
  Returns a sequence of values returned by every parser."
  [ps]
  (if-let [p (first ps)]
    (when-let [x p, xs (sequence (rest ps))]
      (result (cons x xs)))
    (result nil)))

(defn tuple
  "This parser tries to apply argument parsers in order until all of them
  succeeds. Returns a sequence of values returned by every parser. It is a 2+
  arity version of the `sequence` parser."
  [p pp & ps]
  (sequence (cons p (cons pp ps))))

(defn optional
  "This parser tries to apply parser `p`. If `p` fails without consuming input,
  it returns the value `x` (or `nil`), otherwise the value returned by `p`.
  Unlike Haskell's `optional` combinator it does not discard the result of `p`
  and behaves like `option` combinator."
  ([p] (optional p nil))
  ([p x]
   (choice p (result x))))

(defn between
  "This parser parses `open`, followed by `p` and `close`. Returns the value
  returned by `p`."
  ([p around] (between p around around))
  ([p open close]
   (when-let [_ open, x p, _ close]
     (result x))))

(defn skip+
  "This parser applies the parser `p` /one/ or more times, skipping its result."
  [p]
  (after p (skip* p)))

(defn many+
  "This parser applies the parser `p` /one/ or more times. Returns a sequence of
  the returned values of `p`."
  [p]
  (when-let [x p, xs (many* p)]
    (result (cons x xs))))

(defn times
  "This parser parses `n` occurrences of `p`. If `n` is smaller or equal to
  zero, the parser equals to `(return nil)`. Returns a sequence of `n` values
  returned by `p`."
  [n p]
  (sequence (repeat n p)))

(defn sep-by+
  "This parser parses /one/ or more occurrences of `p`, separated by `sep`.
  Returns a sequence of values returned by `p`."
  [p sep]
  (when-let [x p, xs (many* (after sep p))]
    (result (cons x xs))))

(defn sep-by*
  "This parser parses /zero/ or more occurrences of `p`, separated by `sep`.
  Returns a sequence of values returned by `p`."
  [p sep]
  (choice (sep-by+ p sep)
          (result nil)))

(defn sep-by-end+
  "This parser parses /one/ or more occurrences of `p`, separated and ended by
  `sep`. Returns a sequence of values returned by `p`."
  [p sep]
  (many+ (when-let [x p, _ sep]
           (result x))))

(defn sep-by-end*
  "This parser parses /zero/ or more occurrences of `p`, separated and ended by
  `sep`. Returns a sequence of values returned by `p`."
  [p sep]
  (choice (sep-by-end+ p sep)
          (result nil)))

(declare sep-by-end-opt*)

(defn sep-by-end-opt+
  "This parser parses /one/ or more occurrences of `p`, separated and optionally
  ended by `sep`. Returns a sequence of values returned by `p`."
  [p sep]
  (when-let [x p]
    (choice (when-let [_ sep, xs (sep-by-end-opt* p sep)]
              (result (cons x xs)))
            (result [x]))))

(defn sep-by-end-opt*
  "This parser parses /zero/ or more occurrences of `p`, separated and optionally
  ended by `sep`. Returns a sequence of values returned by `p`."
  [p sep]
  (choice (sep-by-end-opt+ p sep)
          (result nil)))

;; TODO: Consider moving chains to separate namespace like kern

(defn chain-left+
  "This parser parses /one/ or more occurrences of `p`, separated by `op`
  Returns a value obtained by a /left/ associative application of all functions
  returned by `op` to the values returned by `p`. This parser can for example be
  used to eliminate left recursion which typically occurs in expression
  grammars."
  [p op]
  (letfn [(more [x]
            (choice (when-let [f op, y p]
                      (more (f x y)))
                    (result x)))]
    (when-let [x p]
      (more x))))

(defn chain-left*
  "This parser parses /zero/ or more occurrences of `p`, separated by `op`.
  Returns a value obtained by a /left/ associative application of all functions
  returned by `op` to the values returned by `p`. If there are zero occurrences
  of `p`, the value `x` is returned."
  [p op x]
  (choice (chain-left+ p op)
          (result x)))

(defn chain-right+
  "This parser parses /one/ or more occurrences of `p`, separated by `op`.
  Returns a value obtained by a /right/ associative application of all functions
  returned by `op` to the values returned by `p`."
  [p op]
  (letfn [(scan []
            (when-let [x p]
              (more x)))
          (more [x]
            (choice (when-let [f op, y (scan)]
                      (result (f x y)))
                    (result x)))]
    (scan)))

(defn chain-right*
  "Parses /zero/ or more occurrences of `p`, separated by `op`. Returns a value
  obtained by a /right/ associative application of all functions returned by
  `op` to the values returned by `p`. If there are no occurrences of `p`, the
  value `x` is returned."
  [p op x]
  (choice (chain-right+ p op)
          (result x)))

;;; Tricky combinators

(def eof
  "This parser only succeeds at the end of the input. This is not a primitive
  parser but it is defined using 'not-followed-by'."
  (-> (not-followed-by any-token)
      (expecting "end of input")))

(defn many-till
  "This parser applies parser `p` /zero/ or more times until parser `end`
  succeeds. Returns a sequence of values returned by `p`."
  [p end]
  (letfn [(scan []
            (choice (after end (result nil))
                    (when-let [x p, xs (scan)]
                      (result (cons x xs)))))]
    (scan)))

(defn debug-state
  "This parser prints the remaining parser state at the time it is invoked. It
  is intended to be used for debugging parsers by inspecting their intermediate
  states."
  [label]
  (choice (silent (when-let [x (silent (many+ any-token))
                             _ (do-parser (println (str label ": " x))
                                          (silent eof))]
                    (fail x)))
          (result nil)))

(defn debug-parser
  "This parser prints to the console the remaining parser state at the time it
  is invoked. It then continues to apply parser `p`, and if `p` fails will
  indicate that the label has been backtracked. It is intended to be used for
  debugging parsers by inspecting their intermediate states."
  [label p]
  (after (debug-state label)
         (choice p, (do-parser (println (str label "  backtracked"))
                               (fail label)))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn new-state
  [input pos user]
  (impl/->State (or (seq input) ())
                pos
                user))

(defn parse
  [p input]
  ;; TODO: Initialize source pos
  (p (new-state input (pos/->IndexPos 0) nil)))

(defn error? [reply] (instance? Failure reply))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
