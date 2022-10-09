(ns strojure.parsesso.core
  (:require [strojure.parsesso.impl.error :as error]
            [strojure.parsesso.impl.parser :as parser]
            [strojure.parsesso.impl.pos :as pos]
            [strojure.parsesso.impl.reply :as reply :include-macros true]
            [strojure.parsesso.impl.state :as state])
  #?(:clj  (:import (clojure.lang ISeq))
     :cljs (:require-macros [strojure.parsesso.core :refer [bind-let do-parser]])))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(def ^{:arglists '([obj])}
  render
  "Returns string representation of the `obj` in parser error messages."
  error/render-object)

(defmacro do-parser
  "Delays the evaluation of a parser that was forward (declare)d and
  it has not been defined yet. For use in (def)s of no-arg parsers,
  since the parser expression evaluates immediately."
  [& body]
  `(fn [state# context#]
     (parser/go (do ~@body) state# context#)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;;; parsers

(defn result
  "This parser always succeeds with value `x` without consuming any input.

  - Fails: never.
  - Consumes: never.
  "
  [x]
  (fn [state context]
    (reply/e-ok context state x)))

(defn fail
  "This parser always fails with message `msg` without consuming any input.

  - Fails: always.
  - Consumes: never.
  "
  ([msg]
   (fn [state context]
     (reply/e-err context (error/message state msg))))
  ([]
   (fail nil)))

(defn fail-unexpected
  "This parser always fails with an unexpected error message `msg` without
  consuming any input.

  - Fails: always.
  - Consumes: never.
  "
  [msg]
  (fn [state context]
    (reply/e-err context (error/unexpected state (or msg (delay (render msg)))))))

(defn expecting
  "This parser behaves as parser `p`, but whenever the parser `p` fails _without
  consuming any input_, it replaces expect error messages with the expect error
  message `msg`.

  This is normally used at the end of a set alternatives where we want to return
  an error message in terms of a higher level construct rather than returning
  all possible characters. For example, if the `expr` parser from the 'maybe'
  example would fail, the error message is: '...: expecting expression'. Without
  the `expecting` combinator, the message would be like '...: expecting \"let\"
  or alphabetic character', which is less friendly.

  The parsers `fail`, `fail-unexpected` and `expecting` are the three parsers
  used to generate error messages. Of these, only `expecting` is commonly used.
  "
  [p msg]
  (fn [state context]
    (letfn [(e-err [e] (reply/e-err context (error/expecting e msg)))]
      (parser/go p state (reply/assign context {reply/e-err e-err})))))

(defn expecting-meta
  "Attaches expecting error message to `obj`, i.e. to token predicate function."
  [obj msg]
  (with-meta obj {::expecting msg}))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn bind
  "This parser applies parser `p` and then parser `(f x)` where x is a return
  value of the parser `p`.

  - Fails: when any of parsers `p` or `(f x)` fails.
  - Consumes: when any of parsers `p` or `(f x)` consumes some input.
  "
  [p f]
  (fn [state context]
    (letfn [(c-ok-p [s x]
              ;; - if (f x) doesn't consume input, but is okay, we still return in the consumed
              ;; continuation
              ;; - if (f x) doesn't consume input, but errors, we return the error in the
              ;; 'consumed-err' continuation
              (parser/go (f x) s (reply/assign context {reply/e-ok (partial reply/c-ok context)
                                                        reply/e-err (partial reply/c-err context)})))
            (e-ok-p [s x]
              ;; - in these cases, (f x) can return as empty
              (parser/go (f x) s context))]
      (parser/go p state (reply/assign context {reply/c-ok c-ok-p
                                                reply/e-ok e-ok-p})))))

(defmacro bind-let
  "Expands into nested bind forms and a function body.

  The pattern:

      (bind p (fn [x]
                (bind q (fn [y]
                          ...
                          (result (f x y ...))))))

  can be more conveniently be written as:

      (bind-let [x p
                 y q
                 ...]
        (result (f x y ...)))
  "
  [[& bindings] & body]
  (let [[sym p :as pair] (take 2 bindings)]
    (assert (= 2 (count pair)) "Requires an even number of forms in bindings")
    (assert (symbol? sym) (str "Requires symbol for binding name: " sym))
    (assert (some? body) "Requires some body")
    (if (= 2 (count bindings))
      `(bind ~p (fn [~sym] ~@body))
      `(bind ~p (fn [~sym] (bind-let ~(drop 2 bindings) ~@body))))))

(defn after
  "This parser tries to apply the parsers in order, until last of them succeeds.
  Returns the value of the last parser, discards result of all preceding
  parsers.

  - Fails: when any of tried parsers fails.
  - Consumes: when any of tried parsers consumes some input.
  "
  ([q p]
   (bind q (fn [_] p)))
  ([q qq p]
   (->> p (after (after q qq))))
  ([q qq qqq & more]
   (reduce after (list* q qq qqq more))))

(defn update-value
  "This parser applies function `f` to the value returned by the parser `p`."
  [p f]
  (bind p (comp result f)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn maybe
  "This parser behaves like parser `p`, except that it pretends that it hasn't
  consumed any input when an error occurs.

  - Fails: when `p` fails.
  - Consumes: when `p` succeeds and consumes some input.

  This combinator is used whenever arbitrary look ahead is needed. Since it
  pretends that it hasn't consumed any input when `p` fails, the `choice`
  combinator will try its second alternative even when the first parser failed
  while consuming input.

  The `maybe` combinator can for example be used to distinguish identifiers and
  reserved words. Both reserved words and identifiers are a sequence of letters.
  Whenever we expect a certain reserved word where we can also expect an
  identifier we have to use the `maybe` combinator. Suppose we write:

      (def identifier
        (many-some char/alpha))

      (def let-expr
        (after (word \"let\")
               ...))

      (def expr
        (-> (choice let-expr
                    identifier)
            (expecting \"expression\"))

  If the user writes \"lexical\", the parser fails with: `unexpected \"x\",
  expecting \"t\" of (word \"let\")`. Indeed, since the `choice` combinator
  only tries alternatives when the first alternative hasn't consumed input, the
  `identifier` parser is never tried (because the prefix \"le\" of the `(word
  \"let\")` parser is already consumed). The right behaviour can be obtained by
  adding the `maybe` combinator:

      (def let-expr
        (after (maybe (word \"let\"))
               ...))
  "
  [p]
  (fn [state context]
    (parser/go p state (reply/assign context {reply/c-err (partial reply/e-err context)}))))

(defn look-ahead
  "Parses `p` without consuming any input. If `p` fails and consumes some input,
  so does `look-ahead`. Combine with `maybe` if this is undesirable.

  - Fails: when `p` fails.
  - Consumes: when `p` fails and consumes some input.
  "
  [p]
  (fn [state context]
    (letfn [(e-ok [_ x] (reply/e-ok context state x))]
      (parser/go p state (reply/assign context {reply/c-ok e-ok,
                                                reply/e-ok e-ok})))))

(letfn
  [(not-followed-by* [q]
     (fn [x]
       (fn [state context]
         (letfn [(e-ok [_ _] (reply/e-err context (if-let [input (seq (state/input state))]
                                                    (error/unexpected state (delay (render (first input))))
                                                    (error/sys-unexpected-eof state))))
                 (e-err [_] (reply/e-ok context state x))]
           (parser/go q state (reply/assign context {reply/c-ok e-ok
                                                     reply/e-ok e-ok
                                                     reply/c-err e-err
                                                     reply/e-err e-err}))))))]
  (defn not-followed-by
    "This parser behaves like parser `p`, except that it only succeeds when parser
    `q` fails. This parser can be used to implement the 'longest match' rule. For
    example, when recognizing keywords (for example `let`), we want to make sure
    that a keyword is not followed by a legal identifier character, in which case
    the keyword is actually an identifier (for example `lets`). We can write this
    behaviour as follows:

        (-> (word \"let\")
            (not-followed-by char/alpha-numeric))

    - Fails:
        - when `p` fails.
        - when `q` succeeds.
    - Consumes:
        - when `p` consumes some input.
    "
    ([p q]
     (bind p (not-followed-by* q)))
    ([q]
     ((not-followed-by* q) nil))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn many-zero
  "This parser applies the parser `p` zero or more times. Returns a sequence of
  the returned values or `p`.

  - Fails: when `p` fails and consumes some input.
  - Consumes: when `p` consumes some input.

      (def identifier
        (bind-let [c char/alpha
                   cs (many-zero (choice char/alpha-numeric
                                         (char/one-of \"_\")))]
          (result (cons c cs))))
  "
  [p]
  (fn [state context]
    (letfn [(walk [xs s x]
              (let [xs (conj! xs x)
                    e-err (fn [_] (reply/c-ok context s (seq (persistent! xs))))]
                (parser/go p s (reply/assign context {reply/c-ok (partial walk xs)
                                                      reply/e-ok parser/e-ok-throw-empty-input
                                                      reply/e-err e-err}))))]
      (parser/go p state (reply/assign context {reply/c-ok (partial walk (transient []))
                                                reply/e-ok parser/e-ok-throw-empty-input
                                                reply/e-err (fn [_] (reply/e-ok context state nil))})))))

(defn many-some
  "This parser applies the parser `p` _one_ or more times. Returns a sequence of
  the returned values of `p`.

  - Fails: when `p` does not succeed at least once.
  - Consumes: when `p` consumes some input.

     (def word
       (many-some char/alpha)
  "
  [p]
  (bind-let [x p, xs (many-zero p)]
    (result (cons x xs))))

(defn skip-zero
  "This parser applies the parser `p` zero or more times, skipping its result.

  - Fails: when `p` fails and consumes some input.
  - Consumes: when `p` consumes some input.

      (def spaces
        (skip-zero char/whitespace))
  "
  [p]
  (fn [state context]
    (letfn [(c-ok [s _]
              (parser/go p s (reply/assign context {reply/c-ok c-ok
                                                    reply/e-ok parser/e-ok-throw-empty-input
                                                    reply/e-err (fn [_] (reply/c-ok context s nil))})))]
      (parser/go p state (reply/assign context {reply/c-ok c-ok
                                                reply/e-ok parser/e-ok-throw-empty-input
                                                reply/e-err (fn [_] (reply/e-ok context state nil))})))))

(defn skip-some
  "This parser applies the parser `p` _one_ or more times, skipping its result.

  - Fails: when `p` does not succeed at least once.
  - Consumes: when `p` consumes some input.
  "
  [p]
  (after p (skip-zero p)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn token
  "This parser accepts a token when `(pred token)` returns logical true, and
  optional expecting `msg`. The `pred` can carry expecting error message in
  `::expecting` metadata. 1-arity behaves as `pred` and can be used in predicate
  composition.

  - Fails: when `(pred token)` return logical false.
  - Consumes: when succeeds.
  "
  {:inline (fn [pred] `(token ~pred nil)) :inline-arities #{1}}
  ([pred] (token pred nil))
  ([pred msg]
   ^{::expecting msg}
   (fn
     ;; Predicate behaviour.
     ([tok] (pred tok))
     ;; Parser behaviour.
     ([state context]
      (if-let [input (-> ^ISeq (state/input state) #?(:clj .seq :cljs -seq))]
        (let [tok (#?(:clj .first :cljs -first) input)]
          (if (pred tok)
            (reply/c-ok context (state/next-state state tok) tok)
            (reply/e-err context (-> (error/sys-unexpected state (delay (render tok)))
                                     (error/expecting (or msg (some-> (meta pred) ::expecting)))))))
        (reply/e-err context (-> (error/sys-unexpected-eof state)
                                 (error/expecting (or msg (some-> (meta pred) ::expecting))))))))))

(defn word
  "Parses a sequence of tokens given by `ts` and returns `ts`. The optional
  function `(test-fn word-token input-token)` is used to match tokens
  differently than simple equality.

  - Fails: when any of tokens don't match the input.
  - Consumes: when at least first token matches the input.
  "
  {:inline (fn [tokens] `(word ~tokens =)) :inline-arities #{1}}
  ([tokens] (word tokens =))
  ([tokens, test-fn]
   (fn [state context]
     (if-let [ws (seq tokens)]
       (loop [^ISeq ws ws
              ^ISeq input (seq (state/input state))
              reply-err reply/e-err]
         (cond
           (not ws)
           (let [new-pos (reduce pos/next-pos (state/pos state) tokens)
                 new-state (state/set-input-pos state input new-pos)]
             (reply/c-ok context new-state tokens))
           (not input)
           (reply-err context (-> (error/sys-unexpected-eof state)
                                  (error/expecting (delay (render tokens)))))
           :else
           (let [w (#?(:clj .first :cljs -first) ws)
                 t (#?(:clj .first :cljs -first) input)]
             (if (test-fn w t)
               (recur (#?(:clj .next :cljs -next) ws)
                      (#?(:clj .next :cljs -next) input)
                      reply/c-err)
               (reply-err context (-> (error/sys-unexpected state (delay (render t)))
                                      (error/expecting (delay (render tokens)))))))))
       (reply/e-ok context state tokens)))))

(def any-token
  "This parser accepts any kind of token. Returns the accepted token.

  - Fails: at the end of input.
  - Consumes: when succeeds.
  "
  (token any?))

(def ^{:arglists '([] [x])}
  eof
  "This parser only succeeds with value `x` at the end of the input.

  - Fails: when input is not completely consumed.
  - Consumes: never.
  "
  (letfn [(eof* [x]
            (fn
              ([] eof)
              ([x] (eof* x))
              ([state context]
               (if-let [input (seq (state/input state))]
                 (reply/e-err context (-> (error/unexpected state (delay (render (first input))))
                                          (error/expecting "end of input")))
                 (reply/e-ok context state x)))))]
    (eof* nil)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;;; combinators

(defn each
  "This parser tries to apply parsers in order until all of them succeeds.
  Returns a sequence of values returned by every parser.

  - Fails: when any of tried parsers fails.
  - Consumes: when any of tried parsers consumes some input.
  "
  [ps]
  (if-let [p (first ps)]
    (bind-let [x p, xs (each (rest ps))]
      (result (cons x xs)))
    (result nil)))

(defn tuple
  "This parser tries to apply argument parsers in order until all of them
  succeeds. Returns a sequence of values returned by every parser. It is a 2+
  arity version of the `each` parser.

  - Fails: when any of tried parsers fails.
  - Consumes: when any of tried parsers consumes some input.
  "
  [p q & ps]
  (each (cons p (cons q ps))))

(defn choice
  "This parser tries to apply the parsers in order, until one of them succeeds.
  Returns the value of the succeeding parser.

  - Fails:
      - when any of tried parsers fails consuming some input.
      - when all tried parsers fail without consuming any input.
  - Consumes:
      - when any of tried parsers consumes some input.

  The parser first applies `p`. If it succeeds, the value of `p` is returned. If
  `p` fails _without consuming any input_, parser `q` is tried and so on.

  The parser is called _predictive_ since `q` is only tried when parser `p`
  didn't consume any input (i.e. the look ahead is 1). This non-backtracking
  behaviour allows for both an efficient implementation of the parser
  combinators and the generation of good error messages.
  "
  ([p q]
   (fn [state context]
     (letfn [(e-err-p [e]
               (letfn [(e-ok-q [s x] (reply/e-ok context s x))
                       (e-err-q [ee] (reply/e-err context (error/merge-errors e ee)))]
                 (parser/go q state (reply/assign context {reply/e-ok e-ok-q
                                                           reply/e-err e-err-q}))))]
       (parser/go p state (reply/assign context {reply/e-err e-err-p})))))
  ([p q qq]
   (-> p (choice q) (choice qq)))
  ([p q qq & more]
   (reduce choice (list* p q qq more))))

(defn optional
  "This parser tries to apply parser `p`. If `p` fails without consuming input,
  it returns the value `x` (or `nil`), otherwise the value returned by `p`.
  Unlike Haskell's `optional` combinator it does not discard the result of `p`
  and behaves like `option` combinator.

  - Fails: when `p` fails and consumes come input.
  - Consumes: when `p` consumes some input.
  "
  ([p] (optional p nil))
  ([p x]
   (choice p (result x))))

(defn between
  "Parses `open`, followed by `p` and `close`. Returns the value returned by `p`.

  - Fails: when any of parses fail.
  - Consumes: in all cases except when `open` fails without consuming any input.

      (defn braces [p]
        (-> p (between (char/one-of \"{\")
                       (char/one-of \"}\"))))
  "
  ([p around] (between p around around))
  ([p open close]
   (bind-let [_ open, x p, _ close]
     (result x))))

(defn times
  "Parses `n` occurrences of `p`. If `n` is smaller or equal to zero, the parser
  equals to `(return nil)`. Returns a sequence of `n` values returned by `p`."
  [n p]
  (each (repeat n p)))

(defn many-till
  "This parser applies parser `p` _zero_ or more times until parser `end`
  succeeds. Returns a sequence of values returned by `p`.

  - Fails:
      - when `p` fails.
      - when `end` does not succeed before end of input.
  - Consumes:
      - when `p` or `end` consumes some input.

      (def simple-comment
        (after (word \"<!--\")
               (many-till any-token (maybe (word \"-->\")))))

  Note the overlapping parsers `any-token` and `(word \"-->\")`, and
  therefore the use of the `maybe` combinator.
  "
  [p end]
  (letfn [(scan [] (choice (after end (result nil))
                           (bind-let [x p, xs (scan)]
                             (result (cons x xs)))))]
    (scan)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn sep-by-some
  "Parses _one_ or more occurrences of `p`, separated by `sep`. Returns a
  sequence of values returned by `p`."
  [p sep]
  (bind-let [x p, xs (many-zero (after sep p))]
    (result (cons x xs))))

(defn sep-by-zero
  "Parses _zero_ or more occurrences of `p`, separated by `sep`. Returns a
  sequence of values returned by `p`.

      (defn comma-sep [p]
        (sep-by-zero p (after (char/one-of \",\")
                              (skip-zero char/whitespace))))
  "
  [p sep]
  (optional (sep-by-some p sep)))

(defn sep-by-end-some
  "Parses _one_ or more occurrences of `p`, separated and ended by `sep`.
  Returns a sequence of values returned by `p`."
  [p sep]
  (many-some (bind-let [x p, _ sep]
               (result x))))

(defn sep-by-end-zero
  "Parses _zero_ or more occurrences of `p`, separated and ended by `sep`.
  Returns a sequence of values returned by `p`."
  [p sep]
  (optional (sep-by-end-some p sep)))

(defn sep-by-opt-end-some
  "Parses _one_ or more occurrences of `p`, separated and optionally ended by
  `sep`. Returns a sequence of values returned by `p`."
  [p sep]
  (bind-let [x p]
    (choice (bind-let [_ sep, xs (optional (sep-by-opt-end-some p sep))]
              (result (cons x xs)))
            (result [x]))))

(defn sep-by-opt-end-zero
  "Parses _zero_ or more occurrences of `p`, separated and optionally ended by
  `sep`. Returns a sequence of values returned by `p`."
  [p sep]
  (optional (sep-by-opt-end-some p sep)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn debug-state
  "This parser prints the remaining parser state at the time it is invoked. It
  is intended to be used for debugging parsers by inspecting their intermediate
  states.

  - Fails: never.
  - Consumes: never.

      (parse (after (char/one-of \"aeiou\")
                    (debug-state \"label\"))
             \"atest\")

      > label: (\\t \\e \\s \\t)
  "
  [label]
  (choice (maybe (bind-let [x (many-some any-token)]
                   (println (str label ": " x))
                   (fail)))
          (result nil)))

(defn debug-parser
  "This parser prints to the console the remaining parser state at the time it
  is invoked. It then continues to apply parser `p`, and if `p` fails will
  indicate that the label has been backtracked. It is intended to be used for
  debugging parsers by inspecting their intermediate states.

  - Fails: when `p` fails.
  - Consumes: when `p` consumes some input.

      (parse (after (char/one-of \"aeiou\")
                    (-> (char/one-of \"nope\")
                        (debug-parser \"one-of-nope\")))
             \"atest\")

      > one-of-nope: (\\t \\e \\s \\t)
      > one-of-nope backtracked

      > error at line 1, column 2:
      > unexpected \"t\"
      > expecting character of \"nope\"
  "
  [p label]
  (after (debug-state label)
         (choice p, (do-parser (println (str label " backtracked"))
                               (fail)))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn parse*
  "Executes parser `p` given `input` sequence of tokens, returns reply record.
  See `parse` for available options."
  {:arglists '([p input]
               [p input {:keys [pos, tab, user-state] :as options}])}
  ([p input]
   (parser/run p (state/init-state input (pos/init-pos nil input) nil)))
  ([p input opts]
   (parser/run p (state/init-state input (pos/init-pos opts input) (:user-state opts)))))

(defn parse
  "Executes parser `p` given `input` sequence of tokens, returns result value or
  throws exception on parsing error.

  Options:

  **`:pos`** The instance of InputPos or keyword for `pos/init-pos` to
  init parser pos. By default pos is initialized to TextPos for string input or
  first token of char type, or IndexPos otherwise.

  **`:tab`** Tab size for TextPos, which is 8 by default.

  **`:user-state`** Initial value of user state.
  "
  {:arglists '([p input]
               [p input {:keys [pos, tab, user-state] :as options}])}
  ([p input]
   (-> (parse* p input) (reply/value)))
  ([p input opts]
   (-> (parse* p input opts) (reply/value))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
