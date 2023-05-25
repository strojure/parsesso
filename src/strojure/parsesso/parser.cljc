(ns strojure.parsesso.parser
  "Main namespace with parsers and their combinators."
  (:refer-clojure :exclude [for])
  (:require [strojure.parsesso.impl.char :as char]
            [strojure.parsesso.impl.error :as error]
            [strojure.parsesso.impl.parser :as parser]
            [strojure.parsesso.impl.pos :as pos]
            [strojure.parsesso.impl.reply :as reply :include-macros true]
            [strojure.parsesso.impl.state :as state])
  #?(:clj  (:import (clojure.lang ISeq))
     :cljs (:require-macros [strojure.parsesso.parser :refer [for do-parser]])))

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

;; ## Parsers ##

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
  all possible characters. For example, if the `expr` parser from the [[maybe]]
  example would fail, the error message is: '...: expecting expression'. Without
  the [[expecting]] combinator, the message would be like '...: expecting
  \"let\" or alphabetic character', which is less friendly.

  The parsers [[fail]], [[fail-unexpected]] and [[expecting]] are the three
  parsers used to generate error messages. Of these, only [[expecting]] is
  commonly used.
  "
  [p msg]
  (fn [state context]
    (letfn [(e-err [e] (reply/e-err context (error/expecting e msg)))]
      (parser/go p state (reply/assign context {reply/e-err e-err})))))

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

(defmacro for
  "Expands into nested bind forms and a function body.

  The pattern:

      (p/bind p (fn [x]
                  (p/bind q (fn [y]
                              ...
                              (p/result (f x y ...))))))

  can be more conveniently be written as:

      (p/for [x p
              y q
             ...]
        (p/result (f x y ...)))
  "
  [[& bindings] & body]
  (let [[sym p :as pair] (take 2 bindings)]
    (assert (= 2 (count pair)) "Requires an even number of forms in bindings")
    (assert (symbol? sym) (str "Requires symbol for binding name: " sym))
    (assert (some? body) "Requires some body")
    (if (= 2 (count bindings))
      `(bind ~p (fn [~sym] ~@body))
      `(bind ~p (fn [~sym] (for ~(drop 2 bindings) ~@body))))))

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

(defn value
  "This parser applies series of functions to the result value of the parser `p`.

  - Fails: when `p` fails.
  - Consumes: when `p` consumes some input.
  "
  ([p f]
   (bind p (fn [x] (result (f x)))))
  ([p f g]
   (bind p (fn [x] (result (g (f x))))))
  ([p f g h]
   (bind p (fn [x] (result (h (g (f x)))))))
  ([p f g h & more]
   (bind p (fn [x] (result (reduce #(%2 %1) x (list* f g h more)))))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn maybe
  "This parser behaves like parser `p`, except that it pretends that it hasn't
  consumed any input when an error occurs.

  - Fails: when `p` fails.
  - Consumes: when `p` succeeds and consumes some input.

  This combinator is used whenever arbitrary look ahead is needed. Since it
  pretends that it hasn't consumed any input when `p` fails, the [[alt]]
  combinator will try its second alternative even when the first parser failed
  while consuming input.

  The [[maybe]] combinator can for example be used to distinguish identifiers
  and reserved words. Both reserved words and identifiers are a sequence of
  letters. Whenever we expect a certain reserved word where we can also expect
  an identifier we have to use the [[maybe]] combinator. Suppose we write:

      (def identifier
        (p/+many char/letter?))

      (def let-expr
        (p/after (p/word \"let\")
                 ...))

      (def expr
        (-> (p/alt let-expr
                   identifier)
            (p/expecting \"expression\"))

  If the user writes \"lexical\", the parser fails with: `unexpected \"x\",
  expecting \"t\" of (word \"let\")`. Indeed, since the [[alt]] combinator only
  tries alternatives when the first alternative hasn't consumed input, the
  `identifier` parser is never tried (because the prefix \"le\" of the `(p/word
  \"let\")` parser is already consumed). The right behaviour can be obtained by
  adding the [[maybe]] combinator:

      (def let-expr
        (p/after (p/maybe (p/word \"let\"))
                 ...))
  "
  [p]
  (fn [state context]
    (parser/go p state (reply/assign context {reply/c-err (partial reply/e-err context)}))))

(defn look-ahead
  "Parses `p` without consuming any input. If `p` fails and consumes some input,
  so does [[look-ahead]]. Combine with [[maybe]] if this is undesirable.

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

        (-> (p/word \"let\")
            (p/not-followed-by char/letter-or-number?))

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

(defn *many
  "This parser applies the parser `p` _zero_ or more times. Returns a sequence
  of the returned values or `p`.

  - Fails: when `p` fails and consumes some input.
  - Consumes: when `p` consumes some input.

  Example:

      (def identifier
        (p/for [c char/letter?
                cs (p/*many (p/alt char/letter-or-number?
                                   (char/is \"_\")))]
          (p/result (cons c cs))))
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

(defn +many
  "This parser applies the parser `p` _one_ or more times. Returns a sequence of
  the returned values of `p`.

  - Fails: when `p` does not succeed at least once.
  - Consumes: when `p` consumes some input.

  Example:

      (def word
        (p/+many char/letter?)
  "
  [p]
  (for [x p, xs (*many p)]
    (result (cons x xs))))

(defn *skip
  "This parser applies the parser `p` _zero_ or more times, skipping its result.

  - Fails: when `p` fails and consumes some input.
  - Consumes: when `p` consumes some input.

  Example:

      (def spaces
        (p/*skip char/white?))
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

(defn +skip
  "This parser applies the parser `p` _one_ or more times, skipping its result.

  - Fails: when `p` does not succeed at least once.
  - Consumes: when `p` consumes some input.
  "
  [p]
  (after p (*skip p)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn token
  "This parser accepts a token when `(pred token)` returns logical true, and
  optional expecting `msg`. 1-arity behaves as `pred` and can be used in
  predicate composition.

  - Fails: when `(pred token)` return logical false.
  - Consumes: when succeeds.
  "
  {:inline (fn [pred] `(token ~pred nil)) :inline-arities #{1}}
  ([pred] (token pred nil))
  ([pred msg]
   (fn
     ;; Predicate behaviour.
     ([tok] (pred tok))
     ;; Parser behaviour.
     ([state context]
      (if-let [input (-> ^ISeq (state/input state) #?(:bb seq :clj .seq :cljs -seq :default seq))]
        (let [tok (#?(:bb first :clj .first :cljs -first :default first) input)]
          (if (pred tok)
            (reply/c-ok context (state/next-state state tok) tok)
            (reply/e-err context (cond-> (error/sys-unexpected state (delay (render tok)))
                                   msg (error/expecting msg)))))
        (reply/e-err context (cond-> (error/sys-unexpected-eof state)
                               msg (error/expecting msg))))))))

(defn token-not
  "This parser accepts a token when `(pred token)` returns logical false, and
  optional expecting `msg`. 1-arity behaves as `(complement pred)` and can be
  used in predicate composition.

  - Fails: when `(pred token)` return logical true.
  - Consumes: when succeeds.
  "
  ([pred]
   (token (complement pred)))
  ([pred msg]
   (token (complement pred) msg)))

(defn register-word-test
  "Associates keyword `k` with test-fn of the [[word]] parser."
  [k, f]
  (parser/register-word-test-fn k f))

(register-word-test :default =)
(register-word-test :ic char/equals-ignorecase)

(defn word
  "Parses a sequence of tokens given by `ts` and returns `ts`. The optional
  function `(test-fn word-token input-token)` is used to match tokens
  differently than simple equality. The `test-fn` can be referred by keyword
  registered using [[register-word-test]]. There are two predefined keywords
  registered: `:default` for `=` and `:ic` for case insensitive char comparison.

  - Fails: when any of tokens don't match the input.
  - Consumes: when at least first token matches the input.

  Example:

      (def let-keyword (p/word \"let\"))

      (def let-keyword-ignorecase (p/word \"let\" :ic))
  "
  {:inline (fn [tokens] `(word ~tokens =)) :inline-arities #{1}}
  ([tokens] (word tokens =))
  ([tokens, test-fn]
   (let [test-fn (cond-> test-fn (keyword? test-fn) (parser/word-test-fn))]
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
             (let [w (#?(:bb first :clj .first :cljs -first :default first) ws)
                   t (#?(:bb first :clj .first :cljs -first :default first) input)]
               (if (test-fn w t)
                 (recur (#?(:bb next :clj .next :cljs -next :default next) ws)
                        (#?(:bb next :clj .next :cljs -next :default next) input)
                        reply/c-err)
                 (reply-err context (-> (error/sys-unexpected state (delay (render t)))
                                        (error/expecting (delay (render tokens)))))))))
         (reply/e-ok context state tokens))))))

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

;; ## Combinators ##

(defn group*
  "This parser tries to apply parsers of `ps` in order until all of them
  succeeds. Returns a sequence of values returned by every parser.

  - Fails: when any of tried parsers fails.
  - Consumes: when any of tried parsers consumes some input.
  "
  [ps]
  (if-let [p (first ps)]
    (for [x p, xs (group* (rest ps))]
      (result (cons x xs)))
    (result nil)))

(defn group
  "This parser tries to apply parsers in order until all of them succeeds.
  Returns a sequence of values returned by every parser.

  - Fails: when any of tried parsers fails.
  - Consumes: when any of tried parsers consumes some input.
  "
  [p q & ps]
  (group* (cons p (cons q ps))))

(defn alt
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
   (-> p (alt q) (alt qq)))
  ([p q qq & more]
   (reduce alt (list* p q qq more))))

(defn option
  "This parser tries to apply parser `p`. If `p` fails without consuming input,
  it returns the value `x` (or `nil`), otherwise the value returned by `p`.

  - Fails: when `p` fails and consumes come input.
  - Consumes: when `p` consumes some input.
  "
  ([p] (option p nil))
  ([p x]
   (alt p (result x))))

(defn between
  "Parses `open`, followed by `p` and `close`. Returns the value returned by `p`.

  - Fails: when any of parses fail.
  - Consumes: in all cases except when `open` fails without consuming any input.

  Example:

      (defn braces [p]
        (-> p (p/between (char/is \"{\")
                         (char/is \"}\"))))
  "
  ([p around] (between p around around))
  ([p open close]
   (for [_ open, x p, _ close]
     (result x))))

(defn times
  "Parses `n` occurrences of `p`. If `n` is smaller or equal to zero, the parser
  equals to `(p/result nil)`. Returns a sequence of `n` values returned by `p`."
  [n p]
  (group* (repeat n p)))

(defn *many-till
  "This parser applies parser `p` _zero_ or more times until parser `end`
  succeeds. Returns a sequence of values returned by `p`.

  - Fails:
      - when `p` fails.
      - when `end` does not succeed before end of input.
  - Consumes:
      - when `p` or `end` consumes some input.

  Example:

      (def simple-comment
        (p/after (p/word \"<!--\")
                 (p/*many-till p/any-token (p/maybe (p/word \"-->\")))))

  Note the overlapping parsers [[any-token]] and `(p/word \"-->\")`, and
  therefore the use of the [[maybe]] combinator.
  "
  [p end]
  (letfn [(scan [] (alt (after end (result nil))
                        (for [x p, xs (scan)]
                          (result (cons x xs)))))]
    (scan)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn +sep-by
  "Parses _one_ or more occurrences of `p`, separated by `sep`. Returns a
  sequence of values returned by `p`."
  [p sep]
  (for [x p, xs (*many (after sep p))]
    (result (cons x xs))))

(defn *sep-by
  "Parses _zero_ or more occurrences of `p`, separated by `sep`. Returns a
  sequence of values returned by `p`.

      (defn comma-sep [p]
        (p/*sep-by p (p/after (char/is \",\")
                              (p/*skip char/white?))))
  "
  [p sep]
  (option (+sep-by p sep)))

(defn +sep-end-by
  "Parses _one_ or more occurrences of `p`, separated and ended by `sep`.
  Returns a sequence of values returned by `p`."
  [p sep]
  (+many (for [x p, _ sep]
           (result x))))

(defn *sep-end-by
  "Parses _zero_ or more occurrences of `p`, separated and ended by `sep`.
  Returns a sequence of values returned by `p`."
  [p sep]
  (option (+sep-end-by p sep)))

(defn +sep-opt-by
  "Parses _one_ or more occurrences of `p`, separated and optionally ended by
  `sep`. Returns a sequence of values returned by `p`."
  [p sep]
  (for [x p]
    (alt (for [_ sep, xs (option (+sep-opt-by p sep))]
           (result (cons x xs)))
         (result [x]))))

(defn *sep-opt-by
  "Parses _zero_ or more occurrences of `p`, separated and optionally ended by
  `sep`. Returns a sequence of values returned by `p`."
  [p sep]
  (option (+sep-opt-by p sep)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;; ## Parser state combinators ##

(defn get-state
  "This parser returns the parser state field `:input`, `:pos` or `:user`.
  Without `field` it returns the parser state record itself."
  {:arglists '([] [:input] [:pos] [:user])}
  ([]
   (fn [state context]
     (reply/e-ok context state state)))
  ([field]
   (fn [state context]
     (reply/e-ok context state (field state)))))

(defn update-state
  "This parser applies function `f` to the parser state field `:input`, `:pos`
  or `:user` and returns modified value. Without `field` it applies `f` to the
  parser state record itself. Suppose that we want to count identifiers in a
  source, we could use the user state as:

      (p/for [x identifier
              _ (p/update-state :user inc)]
        (p/result x))"
  {:arglists '([f] [:input, f] [:pos, f] [:user, f])}
  ([f]
   (fn [state context]
     (let [s (f state)]
       (reply/e-ok context s s))))
  ([field f]
   (fn [state context]
     (let [v (cond-> (f (field state))
               (= :input field) (state/conform-input))]
       (reply/e-ok context (assoc state field v) v)))))

(defn set-state
  "This parser sets the parser state field `:input`, `:pos` or `:user` to `x`.
  Without `field` it sets the parser state record itself to `state`."
  {:arglists '([state] [:input, new-input] [:pos, new-pos] [:user, new-user-state])}
  ([state]
   (update-state (constantly state)))
  ([field x]
   (update-state field (constantly x))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn trace
  "This parser prints the parser state (position, remaining input and user
  state) at the time it is invoked. When `p` is provided it then continues to
  apply parser `p`, and if `p` fails will indicate that the label has been
  backtracked. It is intended to be used for debugging parsers by inspecting
  their intermediate states.

  - Fails: when `p` fails.
  - Consumes: when `p` consumes some input.

  Examples:

      (p/parse (p/after (char/is \"aeiou\")
                        (p/trace \"test-label\"))
               \"atest\")

      > test-label: at line 1, column 2
      >  - input: (\\t \\e \\s \\t)
      >  - user: nil

      (p/parse (p/after (char/is \"aeiou\")
                        (p/trace \"test-label\" (char/is \"nope\")))
               \"atest\")

      > test-label: at line 1, column 2
      >  - input: (\\t \\e \\s \\t)
      >  - user: nil
      > test-label: backtracked

      > error at line 1, column 2:
      > unexpected \"t\"
      > expecting character of \"nope\"
  "
  ([label]
   (fn [state context]
     (println (str label ": at " (state/pos state)
                   "\n - input: " (pr-str (take 20 (state/input state)))
                   "\n - user: " (pr-str (state/user state))))
     (reply/e-ok context state nil)))
  ([label p]
   (after (trace label)
          (alt p, (do-parser (println (str label ": backtracked"))
                             (fail))))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn parse*
  "Executes parser `p` given `input` sequence of tokens, returns reply record.
  See [[parse]] for available `opts`."
  ([p input]
   (parser/run p (state/init-state input (pos/init-pos nil input) nil)))
  ([p input opts]
   (parser/run p (state/init-state input (pos/init-pos opts input) (:user-state opts)))))

(defn parse
  "Executes parser `p` given `input` sequence of tokens, returns result value or
  throws exception on parsing error.

  Options:

  - `:pos` − The instance of InputPos or keyword for `pos/init-pos` to init
             parser pos. By default, pos is initialized to TextPos for string
             input or first token of char type, or IndexPos otherwise.

  - TextPos options:
      - `:tab`  − tab size, default: 8.
      - `:line` − line number, default: 1.
      - `:col`  − column number, default: 1.

  - `:user-state` − Initial value of user state.
  "
  {:arglists '([p input]
               [p input {:keys [pos user-state] :as options}]
               [p input {:keys [tab line col user-state] :as options}])}
  ([p input]
   (-> (parse* p input) (reply/value)))
  ([p input opts]
   (-> (parse* p input opts) (reply/value))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
