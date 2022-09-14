(ns strojure.parsessor.core
  (:require [strojure.parsessor.impl.pos :as pos]
            [strojure.parsessor.impl.reply :as re])
  (:import (clojure.lang IFn)
           (strojure.parsessor.impl.reply Context)))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defprotocol IContinuation
  (continue
    [p state ctx]
    [c]
    "Continues parser call.
    The `(continue p state ctx)` returns continuation of `(p state ctx)`.
    The `(continue c)` invokes executes continuation `c`."))

(deftype Continue [f]
  IContinuation (continue [_] (f)))

;; TODO: Apply continuation to reply context (code smell)?
(extend-type Context
  IContinuation
  (continue [ctx p state]
    ;; TODO: Reuse `(continue p state ctx)`?
    (Continue. (fn [] (p state ctx)))))

#?(:clj (deftype Parser [parser-fn]
          IFn (invoke [_ state ctx] (parser-fn state ctx))
          IContinuation (continue [_ state ctx] (Continue. (fn [] (parser-fn state ctx))))))

(defn parser [parser-fn] (Parser. parser-fn))
(defn parser? [p] (instance? Parser p))

(defrecord State [input pos user])

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defrecord ParseError [pos messages])

(defn new-error-unknown [pos] (ParseError. pos nil))
(defn new-error-message [typ msg pos] (ParseError. pos [[typ msg]]))

(defn error-is-unknown [error] (nil? (:messages error)))

(defn merge-error [e1 e2]
  (let [m1 (:messages :e1), m2 (:messages e2)]
    (cond (nil? m2) e1
          (nil? m1) e2
          :else (let [pos1 (:pos e1)]
                  (case (compare pos1 (:pos e2))
                    1 e1, -1 e2, (ParseError. pos1 (reduce conj m1 m2)))))))

(defn unknown-error [state]
  (new-error-unknown (:pos state)))

(defn sys-unexpect-error [msg pos]
  (new-error-message :msg/sys-unexpect msg pos))

(defn add-error-message
  [err typ msg]
  (update err :messages (fnil conj []) [typ msg]))

(defn set-error-message
  [err typ msg]
  ;; TODO: filter duplicates
  (update err :messages (fnil conj []) [typ msg]))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

#_(defn mkpt
    "k is (fn [state] reply)"
    [f]
    (parser
      (fn [state ctx]
        (let [reply (f state)]
          (if (:consumed reply)
            (if (value-reply? reply)
              (consumed-ok ctx (:value reply) (:state reply) (:error reply))
              (consumed-err ctx (:error reply)))
            (if (value-reply? reply)
              (empty-ok ctx (:value reply) (:state reply) (:error reply))
              (empty-err ctx (:error reply))))))))

#_(defn fmap-reply
    [f reply]
    (cond-> reply
      (value-reply? reply) (update :value f)))

#_(defn parsec-map
    [f p]
    (parser
      (fn [state ctx]
        (-> ctx
            (re/set-consumed-ok (fn [x s e] (consumed-ok ctx (f x) s e)))
            (re/set-empty-ok (fn [x s e] (empty-ok ctx (f x) s e)))
            (continue p state)))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;;; parsers

(defn unexpected
  "The parser @unexpected msg@ always fails with an unexpected error
  message @msg@ without consuming any input.

  The parsers 'fail', ('<?>') and @unexpected@ are the three parsers
  used to generate error messages. Of these, only ('<?>') is commonly
  used. For an example of the use of @unexpected@, see the definition
  of 'Text.Parsec.Combinator.notFollowedBy'."
  [msg]
  (parser
    (fn [state ctx]
      (re/empty-err ctx (new-error-message :msg/un-expect msg (:pos state))))))

(defn return
  [x]
  (parser
    (fn [state ctx]
      (re/empty-ok ctx x state (unknown-error state)))))

(defn bind
  "m - parser, k - (fn [value] parser), returns parser"
  [p f]
  (parser
    (fn [state ctx]
      (-> ctx
          (re/set-consumed-ok
            (fn [x s e]
              (if (error-is-unknown e)
                (continue ctx (f x) s)
                ;; - if (k x) consumes, those go straight up
                ;; - if (k x) doesn't consume input, but is okay, we still return
                ;;   in the consumed continuation
                ;; - if (k x) doesn't consume input, but errors, we return the
                ;;   error in the 'consumed-err' continuation
                (-> ctx
                    (re/set-empty-ok (fn [x s e'] (re/consumed-ok ctx x s (merge-error e e'))))
                    (re/set-empty-err (fn [e'] (re/consumed-err ctx (merge-error e e'))))
                    (continue (f x) s)))))
          (re/set-empty-ok
            (fn [x s e]
              (if (error-is-unknown e)
                (continue ctx (f x) s)
                ;; - in these cases, (k x) can return as empty
                (-> ctx
                    (re/set-empty-ok (fn [x s e'] (re/empty-ok ctx x s (merge-error e e'))))
                    (re/set-empty-err (fn [e'] (re/empty-err ctx (merge-error e e'))))
                    (continue (f x) s)))))
          (continue p state)))))

(defn fail
  "Always fails without consuming any input"
  [msg]
  (parser
    (fn [state ctx]
      (re/empty-err ctx (new-error-message :msg/message msg (:pos state))))))

;; TODO: remove zero?
(declare zero)

;; TODO: remove plus?
(declare plus)

(defn labels
  [p messages]
  (parser
    (fn [state ctx]
      (letfn [(set-expect-errors [e [msg & more :as messages]]
                (cond
                  more, (->> messages (reduce (fn [e msg] (add-error-message e :msg/expect msg)) e))
                  msg,, (set-error-message e :msg/expect msg)
                  :else (set-error-message e :msg/expect "")))]
        (-> ctx
            (re/set-empty-ok (fn [x s e] (re/empty-ok ctx x s (cond-> e (not (error-is-unknown e))
                                                                        (set-expect-errors messages)))))
            (re/set-empty-err (fn [e] (re/empty-err ctx (set-expect-errors e messages))))
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

;; The parser @try p@ behaves like parser @p@, except that it
;; pretends that it hasn't consumed any input when an error occurs.
;;
;; This combinator is used whenever arbitrary look ahead is needed.
;; Since it pretends that it hasn't consumed any input when @p@ fails,
;; the ('<|>') combinator will try its second alternative even when the
;; first parser failed while consuming input.
(defn try*
  [p]
  (parser
    (fn [state ctx]
      (-> ctx
          (re/set-consumed-err (partial re/empty-err ctx))
          (continue p state)))))

(defn look-ahead
  ;; TODO: Update reference to `try`.
  "Parses `p` without consuming any input. If `p` fails and consumes some input,
  so does `look-ahead`. Combine with `try` if this is undesirable."
  [p]
  (parser
    (fn [state ctx]
      (let [val-fn (fn [x _ _] (re/empty-ok ctx x state (new-error-unknown (:pos state))))]
        (-> ctx
            (re/set-consumed-ok val-fn)
            (re/set-empty-ok val-fn)
            (continue p state))))))

(defn- unexpect-error
  [msg pos]
  (new-error-message :msg/sys-unexpect msg pos))

(defn token
  "Returns the parser which accepts a token when `(pred token)` returns logical
  true. The token can be shown in error message using `(msg-fn token)`."
  ([pred] (token pred (partial str "token: ")))
  ;; TODO: split to two versions for get-next-user like in haskell (for performance?)
  ([pred, msg-fn] (token pred msg-fn nil))
  ([pred, msg-fn, user-fn]
   (parser
     (fn [state ctx]
       (if-let [input (seq (:input state))]
         (let [tok (first input)]
           (if (pred tok)
             (let [pos (:pos state)
                   new-input (rest input)
                   new-pos (pos/next-pos pos tok new-input)
                   new-state (->State new-input new-pos (cond->> (:user state)
                                                          user-fn (user-fn pos tok new-input)))]
               (re/consumed-ok ctx tok new-state (new-error-unknown new-pos)))
             (re/empty-err ctx (unexpect-error (delay (msg-fn tok)) (:pos state)))))
         (re/empty-err ctx (unexpect-error "" (:pos state))))))))

(comment
  (def -input "abc")
  (parse (token #(= \a %)) -input)
  (parse (token #(= \b %)) -input)
  (parse (choice (token #(= \a %)) (token #(= \b %))) -input)
  (parse (choice (token #(= \b %)) (token #(= \a %))) -input)
  )

(defn- many-error
  [sym]
  (fn [_ _ _]
    (throw (ex-info (str "Combinator '" sym "' is applied to a parser that accepts an empty string.") {}))))

;; TODO: return nil or [] for empty result?
(defn many
  "Applies the parser `p` zero or more times. Returns a vector of the returned
  values or `p`. Optional `init` is a collection to add values to."
  ([p] (many p []))
  ([p init]
   (parser
     (fn [state ctx]
       (let [ctx (re/set-empty-ok ctx (many-error 'many))
             walk (fn walk [xs x s _e]
                    (let [xs' (conj! xs x)]
                      (-> ctx
                          (re/set-consumed-ok (partial walk xs'))
                          (re/set-empty-err (fn [e] (re/consumed-ok ctx (persistent! xs') s e)))
                          (continue p s))))]
         (-> ctx
             (re/set-consumed-ok (partial walk (transient init)))
             (re/set-empty-err (partial re/consumed-ok ctx init state))
             (continue p state)))))))

(comment
  (def -input "")
  (def -input (seq "abc123"))
  (def -input (seq "123"))
  (def -input (repeat 10000 \a))
  (parse (many (token #(Character/isLetter ^char %))) -input)
  )

(defn skip-many
  "Applies the parser `p` zero or more times, skipping its result."
  [p]
  (parser
    (fn [state ctx]
      (let [ctx (re/set-empty-ok ctx (many-error 'skip-many))
            walk (fn walk [_x s _e]
                   (-> ctx
                       (re/set-consumed-ok walk)
                       (re/set-empty-err (partial re/consumed-ok ctx nil s))
                       (continue p s)))]
        (-> ctx
            (re/set-consumed-ok walk)
            (re/set-empty-err (partial re/consumed-ok ctx nil state))
            (continue p state))))))

(comment
  (def -input "")
  (def -input (seq "abc123"))
  (def -input (seq "123"))
  (def -input (repeat 10000 \a))
  (parse (skip-many (token #(Character/isLetter ^char %))) -input)
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;;; combinators

;; TODO: consider >> in api
(defn >>
  [p1 p2]
  (bind p1 (fn const [_] p2)))

(defmacro when-bind
  [[& bindings] & body]
  ;; TODO: validate macro arguments
  (let [[sym p] (take 2 bindings)]
    (if (= 2 (count bindings))
      `(bind ~p (fn [~sym] (let [p# ~@body]
                             ;; Allow return value directly in body
                             (cond-> p# (not (parser? p#)) (return)))))
      `(bind ~p (fn [~sym] (when-bind ~(drop 2 bindings) ~@body))))))

#_(defmacro with
    [[& bindings] & body]
    (let [[sym p] (take 2 bindings)]
      (if (= 2 (count bindings))
        `(bind ~p (fn [~sym] ~@body))
        `(bind ~p (fn [~sym] (when-bind ~(drop 2 bindings) ~@body))))))

(defn choice
  "Tries to apply the parsers in in order, until one of them succeeds. Returns
  the value of the succeeding parser."
  ([p1 p2]
   (parser
     (fn [state ctx]
       (-> ctx
           (re/set-empty-err (fn [e]
                               (-> ctx
                                   (re/set-empty-ok (fn [x s e'] (re/empty-ok ctx x s (merge-error e e'))))
                                   (re/set-empty-err (fn [e'] (re/empty-err ctx (merge-error e e'))))
                                   (continue p2 state))))
           (continue p1 state)))))
  ([p1 p2 p3]
   (-> (choice p1 p2) (choice p3)))
  ([p1 p2 p3 & more]
   (reduce choice (list* p1 p2 p3 more))))

(defn option
  "Tries to apply parser `p`. If `p` fails without consuming input, it returns
  the value `x`, otherwise the value returned by `p`."
  [x p]
  (choice p (return x)))

(defn optional
  "Tries to apply parser `p`. It will parse `p` or nothing. It only fails if `p`
  fails after consuming input. It discards the result of `p`."
  [p]
  (choice (when-bind [_ p] (return nil)) (return nil)))

(defn between
  "Parses `open`, followed by `p` and `close`. Returns the value returned by `p`."
  ([p around] (between p around around))
  ([p open close]
   (when-bind [_ open, x p, _ close]
     (return x))))

(defn skip-many-1
  "Applies the parser `p` /one/ or more times, skipping its result."
  [p]
  (when-bind [_ p]
    (skip-many p)))

(defn many-1
  "Applies the parser `p` /one/ or more times. Returns a list of the returned
  values of `p`."
  [p]
  (when-bind [x p]
    (many p [x])))

(declare sep-by-1)

;; TODO: return nil or [] for empty result?
(defn sep-by
  "Parses /zero/ or more occurrences of `p`, separated by `sep`. Returns a
  vector of values returned by `p`."
  [p sep]
  (choice (sep-by-1 p sep) (return [])))

(defn sep-by-1
  "Parses /one/ or more occurrences of `p`, separated by `sep`. Returns a vector
  of values returned by `p`."
  [p sep]
  (when-bind [x p]
    (many (>> sep p) [x])))

(declare sep-end-by)

(defn sep-end-by-1
  "Parses /one/ or more occurrences of `p`, separated and optionally ended by
  `sep`. Returns a vector of values returned by `p`."
  [p sep]
  (when-bind [x p]
    (choice (when-bind [_ sep, xs (sep-end-by p sep)]
              ;; TODO: cons?
              (return (cons x xs)))
            (return [x]))))

;; TODO: return nil or [] for empty result?
(defn sep-end-by
  "Parses /zero/ or more occurrences of `p`, separated and optionally ended by
  `sep`. Returns a list of values returned by `p`."
  [p sep]
  (choice (sep-end-by-1 p sep)
          (return [])))

;; | @endBy1 p sep@ parses /one/ or more occurrences of @p@, separated
;; and ended by @sep@. Returns a list of values returned by @p@.
(declare end-by-1)

;; | @endBy p sep@ parses /zero/ or more occurrences of @p@, separated
;; and ended by @sep@. Returns a list of values returned by @p@.
(declare end-by)

;; | @count n p@ parses @n@ occurrences of @p@. If @n@ is smaller or
;; equal to zero, the parser equals to @return []@. Returns a list of
;; @n@ values returned by @p@.
(declare times)
#_(declare repeat)

;; | @chainr p op x@ parses /zero/ or more occurrences of @p@,
;; separated by @op@ Returns a value obtained by a /right/ associative
;; application of all functions returned by @op@ to the values returned
;; by @p@. If there are no occurrences of @p@, the value @x@ is
;; returned.
(declare chain-right)

;; | @chainl p op x@ parses /zero/ or more occurrences of @p@,
;; separated by @op@. Returns a value obtained by a /left/ associative
;; application of all functions returned by @op@ to the values returned
;; by @p@. If there are zero occurrences of @p@, the value @x@ is
;; returned.
(declare chain-left)

;; | @chainl1 p op@ parses /one/ or more occurrences of @p@,
;; separated by @op@ Returns a value obtained by a /left/ associative
;; application of all functions returned by @op@ to the values returned
;; by @p@. This parser can for example be used to eliminate left
;; recursion which typically occurs in expression grammars.
(declare chain-left-1)

;; | @chainr1 p op x@ parses /one/ or more occurrences of |p|,
;; separated by @op@ Returns a value obtained by a /right/ associative
;; application of all functions returned by @op@ to the values returned
;; by @p@.
(declare chain-right-1)

;;; Tricky combinators

;; | The parser @anyToken@ accepts any kind of token. It is for example
;; used to implement 'eof'. Returns the accepted token.
(declare any-token)

;; | This parser only succeeds at the end of the input. This is not a
;; primitive parser but it is defined using 'notFollowedBy'.
(defn eof []
  #_(-> (not-followed-by any-token)
        (label "end of input")))

;; | @notFollowedBy p@ only succeeds when parser @p@ fails. This parser
;; does not consume any input. This parser can be used to implement the
;; \'longest match\' rule. For example, when recognizing keywords (for
;; example @let@), we want to make sure that a keyword is not followed
;; by a legal identifier character, in which case the keyword is
;; actually an identifier (for example @lets@).
(declare not-followed-by)

;; | @manyTill p end@ applies parser @p@ /zero/ or more times until
;; parser @end@ succeeds. Returns the list of values returned by @p@.
(declare many-till)

;; | @parserTrace label@ is an impure function, implemented with "Debug.Trace" that
;; prints to the console the remaining parser state at the time it is invoked.
;; It is intended to be used for debugging parsers by inspecting their intermediate states.
(declare trace-parser)

;; | @parserTraced label p@ is an impure function, implemented with "Debug.Trace" that
;; prints to the console the remaining parser state at the time it is invoked.
;; It then continues to apply parser @p@, and if @p@ fails will indicate that
;; the label has been backtracked.
(declare traced-parser)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn parse
  [p input]
  ;; TODO: Initialize source pos
  (loop [reply (p (State. (seq input) 1 nil) (re/new-context))]
    (if (instance? Continue reply)
      (recur (continue reply))
      reply)))

(comment
  (def -input (seq "a"))
  (def -input (seq "abc123"))
  (def -input (seq "a1b2c3"))
  (def -input (seq "a1b2c"))
  (def -input (seq "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))
  (def -input (seq "123"))
  (def -input (repeat 10000 \a))

  (-> (many (token #(Character/isLetter ^char %)))
      (parse -input))
  (-> (many-1 (token #(Character/isLetter ^char %)))
      (parse -input))

  (-> (>> (token #(Character/isLetter ^char %))
          (token #(Character/isDigit ^char %)))
      (parse -input))
  (-> (sep-by (token #(Character/isLetter ^char %))
              (token #(Character/isDigit ^char %)))
      (parse -input))
  (-> (sep-by-1 (token #(Character/isLetter ^char %))
                (token #(Character/isDigit ^char %)))
      (parse -input))
  (-> (sep-end-by (token #(Character/isLetter ^char %))
                  (token #(Character/isDigit ^char %)))
      (parse -input))
  (-> (sep-end-by-1 (token #(Character/isLetter ^char %))
                    (token #(Character/isDigit ^char %)))
      (parse -input))

  (parse (optional (fail :ok)) nil)

  (def -input (seq "[]"))
  (def -input (seq "[abc]"))
  (def -input (seq "[abc123]"))
  (-> (many (token #(Character/isLetter ^char %)))
      (between (token #(= \[ %)) (token #(= \] %)))
      (parse -input))

  (parse (return :ok) -input)
  (parse (bind (return :ok) #(return (str %))) -input)
  (parse (bind (fail :x1) (fn [x] (return :x2))) -input)
  (parse (when-bind [x (return :x)
                     y (return {:y x})]
           y)
         -input)

  (parse (fail "oops") -input)
  (parse (choice (return :ok) (fail "oops")) -input)
  (parse (choice (fail "oops") (return :ok)) -input)
  (parse (choice (fail "oops") (fail "oops2") (return :ok)) -input)
  (parse (choice (fail "oops") (fail "oops2")) -input)
  (def -p (choice (fail "oops") (return :ok)))
  (def -p (choice (fail "oops") (fail "oops2") (return :ok)))
  (def -p (choice (return :ok) (fail "oops") (fail "oops2")))
  (parse -p -input)
  (parse (try* (fail "oops")) -input)
  (parse (try* (return :ok)) -input)

  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
