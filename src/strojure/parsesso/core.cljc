(ns strojure.parsesso.core
  (:refer-clojure :exclude [sequence when-let])
  (:require [strojure.parsesso.impl.error :as error]
            [strojure.parsesso.impl.parser :as parser #?@(:cljs (:refer [Parser]))]
            [strojure.parsesso.impl.pos :as pos]
            [strojure.parsesso.impl.reply :as reply #?@(:cljs (:refer [Failure replace]))]
            [strojure.parsesso.impl.state :as state])
  #?(:clj  (:import (clojure.lang ISeq)
                    (strojure.parsesso.impl.parser Parser)
                    (strojure.parsesso.impl.reply Failure))
     :cljs (:require-macros [strojure.parsesso.core :refer [do-parser when-let]])))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn parser
  "Wraps function `(fn [state context])` in the instance of `Parser`."
  [f]
  (parser/->Parser f))

(defn parser? [p] (instance? Parser p))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;;; parsers

(defn result
  "This parser always succeeds with value `x` without consuming any input."
  [x]
  (parser
    (fn [state context]
      (reply/e-ok context x state nil))))

(defn fail
  "This parser always fails with message `msg` without consuming any input."
  [msg]
  (parser
    (fn [state context]
      (reply/e-err context (error/message state msg)))))

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
      (letfn [(e-ok [x s e]
                (reply/e-ok context x s (some-> e (error/with-expecting msg))))
              (e-err [e]
                (reply/e-err context (error/with-expecting e msg)))]
        (p state (reply/replace context {reply/e-ok e-ok
                                         reply/e-err e-err}))))))

(defn unexpected
  "This parser always fails with an unexpected error message `msg` without
  consuming any input.

  The parsers 'fail', 'expecting' and `unexpected` are the three parsers used to
  generate error messages. Of these, only `expecting` is commonly used. For an
  example of the use of `unexpected`, see the definition of `not-followed-by`."
  [msg]
  (parser
    (fn [state context]
      (reply/e-err context (error/unexpected state msg)))))

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
      (p state (reply/replace context {reply/c-err (partial reply/e-err context)})))))

(defn look-ahead
  "This parser parses `p` without consuming any input. If `p` fails and consumes
  some input, so does `look-ahead`. Combine with `accept` if this is
  undesirable."
  [p]
  (parser
    (fn [state context]
      (letfn [(e-ok [x _ _]
                (reply/e-ok context x state nil))]
        (p state (reply/replace context {reply/c-ok e-ok
                                         reply/e-ok e-ok}))))))

(defn token-fn
  "This parser accepts a token when `(pred token)` returns logical true. The
  token can be shown in error message using `(msg-fn token)`."
  [{:keys [msg-fn, user-fn] :or {msg-fn pr-str}}]
  (if-not user-fn
    (fn [pred]
      (parser
        (fn [state context]
          (if-let [input (-> ^ISeq (state/input state) #?(:clj .seq :cljs -seq))]
            (let [tok (#?(:clj .first :cljs -first) input)]
              (if (pred tok)
                (reply/c-ok context tok (state/next-state state tok) nil)
                (reply/e-err context (error/sys-unexpected state (delay (msg-fn tok))))))
            (reply/e-err context (error/sys-unexpected state))))))
    (fn [pred]
      (parser
        (fn [state context]
          (if-let [input (-> ^ISeq (state/input state) #?(:clj .seq :cljs -seq))]
            (let [tok (#?(:clj .first :cljs -first) input)]
              (if (pred tok)
                (reply/c-ok context tok (state/next-state state tok user-fn) nil)
                (reply/e-err context (error/sys-unexpected state (delay (msg-fn tok))))))
            (reply/e-err context (error/sys-unexpected state))))))))

(def token
  "This parser accepts a token when `(pred token)` returns logical true. See
  `token-fn` for customized version of the parser."
  (token-fn {}))

(def any-token
  "This parser accepts any kind of token. Returns the accepted token."
  (token any?))

(defn many
  "This parser applies the parser `p` zero or more times. Returns a sequence of
  the returned values or `p`."
  [p]
  (parser
    (fn [state context]
      (letfn [(walk [xs x s _e]
                (let [xs (conj! xs x)
                      e-err (fn e-err [e]
                              (reply/c-ok context (seq (persistent! xs)) s e))]
                  (p s (reply/replace context {reply/c-ok (partial walk xs)
                                               reply/e-ok parser/e-ok-throw-empty-input
                                               reply/e-err e-err}))))]
        (p state (reply/replace context {reply/c-ok (partial walk (transient []))
                                         reply/e-ok parser/e-ok-throw-empty-input
                                         reply/e-err (partial reply/e-ok context nil state)}))))))

(defn skip-many
  "This parser applies the parser `p` zero or more times, skipping its result."
  [p]
  (parser
    (fn [state context]
      (letfn [(walk [_x s _e]
                (p s (reply/replace context {reply/c-ok walk
                                             reply/e-ok parser/e-ok-throw-empty-input
                                             reply/e-err (partial reply/c-ok context nil s)})))]
        (p state (reply/replace context {reply/c-ok walk
                                         reply/e-ok parser/e-ok-throw-empty-input
                                         reply/e-err (partial reply/e-ok context nil state)}))))))

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
                          (reply/c-ok context x s (cond->> ee e (error/merge-errors e))))
                        (c-err-fx [ee]
                          (reply/c-err context (cond->> ee e (error/merge-errors e))))]
                  ((f x) s (reply/replace context {reply/e-ok c-ok-fx
                                                   reply/e-err c-err-fx}))))
              (e-ok-p [x s e]
                (if e
                  ;; - in these cases, (f x) can return as empty
                  (letfn [(e-ok-fx [x s ee]
                            (reply/e-ok context x s (error/merge-errors e ee)))
                          (e-err-fx [ee]
                            (reply/e-err context (error/merge-errors e ee)))]
                    ((f x) s (reply/replace context {reply/e-ok e-ok-fx
                                                     reply/e-err e-err-fx})))
                  ((f x) s context)))]
        (p state (reply/replace context {reply/c-ok c-ok-p
                                         reply/e-ok e-ok-p}))))))

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
                           (reply/e-ok context x s (error/merge-errors e ee)))
                         (e-err-pp [ee]
                           (reply/e-err context (error/merge-errors e ee)))]
                   (pp state (reply/replace context {reply/e-ok e-ok-pp
                                                     reply/e-err e-err-pp}))))]
         (p state (reply/replace context {reply/e-err e-err-p}))))))
  ([p pp ppp]
   (-> p (choice pp) (choice ppp)))
  ([p pp ppp & more]
   (reduce choice (list* p pp ppp more))))

(defn fmap
  "This parser applies function `f` to the value returned by the parser `p`."
  [f p]
  (bind p (comp result f)))

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

(defn token-seq
  "This parser parses a sequence of tokens given by `xs` using function `pf` to
  convert every token to its parser. Returns `xs`."
  ([xs]
   (token-seq xs (fn [tok]
                   (-> (token (partial = tok))
                       (expecting (delay (str (pr-str tok) " of (token-seq " (pr-str xs) ")")))))))
  ([xs pf]
   (if-let [ts (seq xs)]
     (after (reduce after (map pf ts))
            (result xs))
     (result xs))))

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

(defn some-skip-many
  "This parser applies the parser `p` /one/ or more times, skipping its result."
  [p]
  (after p (skip-many p)))

(defn some-many
  "This parser applies the parser `p` /one/ or more times. Returns a sequence of
  the returned values of `p`."
  [p]
  (when-let [x p, xs (many p)]
    (result (cons x xs))))

(defn times
  "This parser parses `n` occurrences of `p`. If `n` is smaller or equal to
  zero, the parser equals to `(return nil)`. Returns a sequence of `n` values
  returned by `p`."
  [n p]
  (sequence (repeat n p)))

(defn some-sep-by
  "This parser parses /one/ or more occurrences of `p`, separated by `sep`.
  Returns a sequence of values returned by `p`."
  [p sep]
  (when-let [x p, xs (many (after sep p))]
    (result (cons x xs))))

(defn sep-by
  "This parser parses /zero/ or more occurrences of `p`, separated by `sep`.
  Returns a sequence of values returned by `p`."
  [p sep]
  (optional (some-sep-by p sep)))

(defn some-sep-by-end
  "This parser parses /one/ or more occurrences of `p`, separated and ended by
  `sep`. Returns a sequence of values returned by `p`."
  [p sep]
  (some-many (when-let [x p, _ sep]
               (result x))))

(defn sep-by-end
  "This parser parses /zero/ or more occurrences of `p`, separated and ended by
  `sep`. Returns a sequence of values returned by `p`."
  [p sep]
  (optional (some-sep-by-end p sep)))

(defn some-sep-by-opt-end
  "This parser parses /one/ or more occurrences of `p`, separated and optionally
  ended by `sep`. Returns a sequence of values returned by `p`."
  [p sep]
  (when-let [x p]
    (choice (when-let [_ sep, xs (optional (some-sep-by-opt-end p sep))]
              (result (cons x xs)))
            (result [x]))))

(defn sep-by-opt-end
  "This parser parses /zero/ or more occurrences of `p`, separated and optionally
  ended by `sep`. Returns a sequence of values returned by `p`."
  [p sep]
  (optional (some-sep-by-opt-end p sep)))

;; TODO: Consider moving chains to separate namespace like kern

(defn some-chain-left
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

(defn chain-left
  "This parser parses /zero/ or more occurrences of `p`, separated by `op`.
  Returns a value obtained by a /left/ associative application of all functions
  returned by `op` to the values returned by `p`. If there are zero occurrences
  of `p`, the value `x` is returned."
  [p op x]
  (optional (some-chain-left p op) x))

(defn some-chain-right
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

(defn chain-right
  "Parses /zero/ or more occurrences of `p`, separated by `op`. Returns a value
  obtained by a /right/ associative application of all functions returned by
  `op` to the values returned by `p`. If there are no occurrences of `p`, the
  value `x` is returned."
  [p op x]
  (optional (some-chain-right p op) x))

;;; Tricky combinators

(defn not-followed-by
  "This parser behaves like parser `p`, except that it only succeeds when parser
  `q` fails. This parser can be used to implement the 'longest match' rule. For
  example, when recognizing keywords (for example `let`), we want to make sure
  that a keyword is not followed by a legal identifier character, in which case
  the keyword is actually an identifier (for example `lets`). We can write this
  behaviour as follows:

      (-> (text/char-seq \"let\")
          (not-followed-by alpha-numeric))
  "
  [p q]
  (->> (fn [px]
         (parser
           (fn [state context]
             (letfn [(e-ok [qx _s _e]
                       (reply/e-err context (error/unexpected state (delay (pr-str qx)))))
                     (e-err [_e]
                       (reply/e-ok context px state nil))]
               (q state (reply/replace context {reply/c-ok e-ok
                                                reply/e-ok e-ok
                                                reply/c-err e-err
                                                reply/e-err e-err}))))))
       (bind p)))

(def eof
  "This parser only succeeds with value `::eof` at the end of the input. This is
  not a primitive parser, but it is defined using 'not-followed-by'."
  (-> (result ::eof)
      (not-followed-by any-token)
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
  (choice (silent (when-let [x (silent (some-many any-token))
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
  (state/->State (or (seq input) ())
                 pos
                 user))

(defn parse
  [p input]
  ;; TODO: Initialize source pos
  (p (new-state input (pos/->IndexPos 0) nil)))

(defn error? [reply] (instance? Failure reply))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
