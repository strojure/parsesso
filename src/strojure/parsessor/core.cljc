(ns strojure.parsessor.core
  (:require [strojure.parsessor.impl.state :as state])
  #?(:clj (:import (clojure.lang IFn))))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defrecord ParseError [pos msgs])
(defrecord Message [t msg])

#?(:clj (defrecord Parser [parser-fn]
          IFn
          (invoke [_ state reps] (parser-fn state reps))))

(defn parser [parser-fn] (Parser. parser-fn))

(defprotocol IReplyMethods
  (consumed-ok [_ value state error])
  (empty-ok,,, [_ value state error])
  (consumed-err [_ error])
  (empty-err,,, [_ error]))

(defrecord ReplyMethods [consumed-ok-fn, empty-ok-fn, consumed-err-fn, empty-err-fn]
  IReplyMethods
  (consumed-ok [_ value state error] (consumed-ok-fn value state error))
  (empty-ok,,, [_ value state error] (empty-ok-fn value state error))
  (consumed-err [_ error] (consumed-err-fn error))
  (empty-err,,, [_ error] (empty-err-fn error)))

(defn init-reps
  []
  (ReplyMethods. (fn consumed-ok [value state error] (ReplyOk. value state error true))
                 (fn empty-ok,,, [value state error] (ReplyOk. value state error false))
                 (fn consumed-err [error] (ReplyError. error true))
                 (fn empty-err,,, [error] (ReplyError. error false))))

(defn set-reps
  ([reps k1 v1]
   (-> reps (assoc k1 v1)))
  ([reps k1 v1, k2 v2]
   (-> reps (assoc k1 v1) (assoc k2 v2))))

(defn new-error-unknown [pos] (ParseError. pos nil))
(defn new-error-message [typ msg pos] (ParseError. pos [[typ msg]]))

(defn error-is-unknown [error] (empty? (:msgs error)))

(defn merge-error [e1 e2]
  ;; TODO: merge errors
  (ParseError. (:pos e1) (into (or (:msgs e1) []) (:msgs e2))))

(defn unknown-error [state]
  (new-error-unknown (:pos state)))

(defn sys-unexpect-error [msg pos]
  (new-error-message :msg/sys-unexpect msg pos))

(defn unexpected
  "The parser @unexpected msg@ always fails with an unexpected error
  message @msg@ without consuming any input.

  The parsers 'fail', ('<?>') and @unexpected@ are the three parsers
  used to generate error messages. Of these, only ('<?>') is commonly
  used. For an example of the use of @unexpected@, see the definition
  of 'Text.Parsec.Combinator.notFollowedBy'."
  [msg]
  (parser
    (fn [state reps]
      (empty-err reps (new-error-message :msg/un-expect msg (:pos state))))))

(defrecord ReplyOk [value state error consumed])
(defrecord ReplyError [error consumed])
(defrecord State [input pos user])

(defn reply-ok? [reply] (instance? ReplyOk reply))
(defn reply-error? [reply] (instance? ReplyError reply))

(defn parse*
  ([p state] (parse* p state (init-reps)))
  ([p state reps] (p state reps))
  ([p state reps k1 f1] (p state (set-reps reps k1 f1)))
  ([p state reps k1 f1 k2 f2] (p state (set-reps reps k1 f1 k2 f2))))

(defn mkpt
  [k]
  (parser
    (fn [state reps]
      (let [reply (k state)]
        (if (:consumed reply)
          (if (reply-ok? reply)
            (consumed-ok reps (:value reply) (:state reply) (:error reply))
            (consumed-err reps (:error reply)))
          (if (reply-ok? reply)
            (empty-ok reps (:value reply) (:state reply) (:error reply))
            (empty-err reps (:error reply))))))))

(defn fmap-reply
  [f reply]
  (cond-> reply
    (reply-ok? reply) (update :value f)))

(defn parsec-map
  [f p]
  (parser
    (fn [state reps]
      (parse* p state reps
              :consumed-ok-fn (fn [x s e] (consumed-ok reps (f x) s e))
              :empty-ok-fn (fn [x s e] (empty-ok reps (f x) s e))))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;;; parsers

(defn return
  [x]
  (parser
    (fn [state reps]
      (empty-ok reps x state (unknown-error state)))))

(defn bind
  "m - parser, k - (fn [value] parser)"
  [m k]
  (parser
    (fn [state reps]
      (parse* m state reps
              :consumed-ok-fn
              (fn [x s e]
                (parse* (k x) s (cond-> reps
                                  (not (error-is-unknown e))
                                  ;; - if (k x) consumes, those go straight up
                                  ;; - if (k x) doesn't consume input, but is okay, we still return
                                  ;;   in the consumed continuation
                                  ;; - if (k x) doesn't consume input, but errors, we return the
                                  ;;   error in the 'consumed-error' continuation
                                  (set-reps :empty-ok-fn (fn [x s e'] (consumed-ok reps x s (merge-error e e')))
                                            :empty-err-fn (fn [e'] (consumed-err reps (merge-error e e')))))))
              :empty-ok-fn
              (fn [x s e]
                (parse* (k x) s (cond-> reps
                                  (not (error-is-unknown e))
                                  ;; - in these cases, (k x) can return as empty
                                  (set-reps :empty-ok-fn (fn [x s e'] (empty-ok reps x s (merge-error e e')))
                                            :empty-err-fn (fn [e'] (empty-err reps (merge-error e e')))))))))))

(defn fail [msg]
  (parser
    (fn [state reps]
      (empty-err reps (new-error-message :msg/message msg (:pos state))))))

;; always fails without consuming any input
(declare zero)

(declare plus)

(declare label)

(declare choice #_(choice [p1 p2]))

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
    (fn [state reps]
      (parse* p state reps :consumed-err-fn (:empty-err-fn reps)))))

;; | @lookAhead p@ parses @p@ without consuming any input.
;;
;; If @p@ fails and consumes some input, so does @lookAhead@. Combine with 'try'
;; if this is undesirable.
(declare look-ahead)

(declare token, token-prim, token-prim-ex)

(declare many)

(declare skip-many)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;;; combinators

;; | @choice ps@ tries to apply the parsers in the list @ps@ in order,
;; until one of them succeeds. Returns the value of the succeeding
;; parser.
(declare choice #_(choice [& ps]))

;; | @option x p@ tries to apply parser @p@. If @p@ fails without
;; consuming input, it returns the value @x@, otherwise the value
;; returned by @p@.
(defn option
  [x p]
  (choice p (return x)))

;; | @optional p@ tries to apply parser @p@.  It will parse @p@ or nothing.
;; It only fails if @p@ fails after consuming input. It discards the result
;; of @p@.
(declare optional)

;; | @between open close p@ parses @open@, followed by @p@ and @close@.
;; Returns the value returned by @p@.
(declare between #_(between [open close p]))

;; | @skipMany1 p@ applies the parser @p@ /one/ or more times, skipping
;; its result.
(declare skip-many-1)

;; | @many1 p@ applies the parser @p@ /one/ or more times. Returns a
;; list of the returned values of @p@.
(declare many-1)

;; | @sepBy p sep@ parses /zero/ or more occurrences of @p@, separated
;; by @sep@. Returns a list of values returned by @p@.
(declare sep-by)

;; | @sepBy1 p sep@ parses /one/ or more occurrences of @p@, separated
;; by @sep@. Returns a list of values returned by @p@.
(declare sep-by-1)

;; | @sepEndBy1 p sep@ parses /one/ or more occurrences of @p@,
;; separated and optionally ended by @sep@. Returns a list of values
;; returned by @p@.
(declare sep-end-by-1)

;; | @sepEndBy p sep@ parses /zero/ or more occurrences of @p@,
;; separated and optionally ended by @sep@, ie. haskell style
;; statements. Returns a list of values returned by @p@.
(declare sep-end-by)

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

#_(defprotocol IParser
    (parse* [this state])
    (parser? [this]))

#_(defrecord Parser [f]
    IParser
    (parser? [_] true)
    (parse* [_ state] (f state)))

#_(defn parse-pred [pred state]
    (if-let [input (seq (state/input state))]
      (let [c (first input)]
        (if (pred c)
          (state/increment state c)
          (state/->Failure pred state ::unexpected "Unexpected")))
      (state/->Failure pred state :eof "End of input")))

#_(:clj (extend-protocol IParser
          Fn
          (parser? [_] true)
          (parse* [pred state] (parse-pred pred state))
          Object
          (parser? [_] false)
          nil
          (parser? [_] false))
    :cljs (extend-protocol IParser
            function
            (parser? [_] true)
            (parse* [pred state] (parse-pred pred state))
            default
            (parser? [_] false)))

#_(defn parse-state*
    [p state]
    (loop [state state, res (parse* p state)]
      (if (parser? res)
        (recur state (parse* res state))
        res)))

#_(defn many
    "Parses p zero or more times; returns the result(s) in a vector. It stops when
    p fails, but this parser succeeds."
    [p]
    (Parser. (fn [state]
               (loop [res (parse-state* p state) vs (transient [])]
                 (if (state/state? res)
                   (recur (parse-state* p res) (conj! vs (:-value res)))
                   (state/set-value (:state res) (persistent! vs)))))))

#_(defn parse
    [p input]
    (parse-state* p (state/->State (seq input) nil nil)))

(defn parse
  [p input]
  (parse* p (State. (seq input) nil nil)))

(comment
  (def -input (seq "a"))
  (def -input (seq "abc123"))
  (def -input (seq "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))
  (def -input (seq "123"))

  (parse (return :ok) -input)
  (parse (bind (return :ok) #(return (str %))) -input)
  (parse (fail "oops") -input)
  (parse (try* (fail "oops")) -input)
  (parse (try* (return :ok)) -input)

  (parser? letter?)
  (defn letter? [c] (Character/isLetter ^Character c))
  (defn letter? [c] (Character/isLetter ^Character c))
  (defn letter? [c] (= "a" c))
  (parse letter? -input)
  (parse (many letter?) -input)
  (seq -input)
  (parse* letter? (state/->State -input nil nil))
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
