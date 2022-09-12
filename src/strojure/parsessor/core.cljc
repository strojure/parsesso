(ns strojure.parsessor.core
  (:require [strojure.parsessor.impl.state :as state])
  #?(:clj (:import (clojure.lang IFn))))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

#?(:clj (defrecord Parser [parser-fn]
          IFn
          (invoke [_ state ret] (parser-fn state ret))))

(defn parser [parser-fn] (Parser. parser-fn))
(defn parser? [p] (instance? Parser p))

(defrecord ValueReply [value consumed state error])
(defrecord ErrorReply [error consumed])

(defn value-reply? [reply] (instance? ValueReply reply))
(defn error-reply? [reply] (instance? ErrorReply reply))

(defrecord State [input pos user])

(defprotocol IReplyApi
  (ret-val-consumed [_ value state error])
  (ret-val-empty,,, [_ value state error])
  (ret-err-consumed [_ error])
  (ret-err-empty,,, [_ error]))

#_:clj-kondo/ignore
(defrecord ReplyApi [ret-val-consumed, ret-val-empty, ret-err-consumed, ret-err-empty]
  IReplyApi
  (ret-val-consumed [_ value state error] (ret-val-consumed value state error))
  (ret-val-empty,,, [_ value state error] (ret-val-empty value state error))
  (ret-err-consumed [_ error] (ret-err-consumed error))
  (ret-err-empty,,, [_ error] (ret-err-empty error)))

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
                    1 e1, -1 e2
                    (ParseError. pos1 (reduce conj m1 m2)))))))

(defn unknown-error [state]
  (new-error-unknown (:pos state)))

(defn sys-unexpect-error [msg pos]
  (new-error-message :msg/sys-unexpect msg pos))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn- init-reply-api
  []
  (ReplyApi. (fn ret-val-consumed [value state error] (ValueReply. value true state error))
             (fn ret-val-empty,,, [value state error] (ValueReply. value false state error))
             (fn ret-err-consumed [error] (ErrorReply. error true))
             (fn ret-err-empty,,, [error] (ErrorReply. error false))))

(defn run-parser
  ([p state] (run-parser p state (init-reply-api)))
  ([p state ret] (p state ret))
  ([p state ret k1 f1] (p state (-> ret (assoc k1 f1))))
  ([p state ret k1 f1 k2 f2] (p state (-> ret (assoc k1 f1) (assoc k2 f2)))))

(defn mkpt
  "k is (fn [state] reply)"
  [k]
  (parser
    (fn [state ret]
      (let [reply (k state)]
        (if (:consumed reply)
          (if (value-reply? reply)
            (ret-val-consumed ret (:value reply) (:state reply) (:error reply))
            (ret-err-consumed ret (:error reply)))
          (if (value-reply? reply)
            (ret-val-empty ret (:value reply) (:state reply) (:error reply))
            (ret-err-empty ret (:error reply))))))))

(defn fmap-reply
  [f reply]
  (cond-> reply
    (value-reply? reply) (update :value f)))

(defn parsec-map
  [f p]
  (parser
    (fn [state ret]
      (run-parser p state ret
        :ret-val-consumed (fn [x s e] (ret-val-consumed ret (f x) s e))
        :ret-val-empty (fn [x s e] (ret-val-empty ret (f x) s e))))))

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
    (fn [state ret]
      (ret-err-empty ret (new-error-message :msg/un-expect msg (:pos state))))))

(defn return
  [x]
  (parser
    (fn [state ret]
      (ret-val-empty ret x state (unknown-error state)))))

(defn bind
  "m - parser, k - (fn [value] parser), returns parser"
  [p f]
  (parser
    (fn [state ret]
      (run-parser p state ret
        :ret-val-consumed (fn [x s e]
                            (if (error-is-unknown e)
                              (run-parser (f x) s ret)
                              ;; - if (k x) consumes, those go straight up
                              ;; - if (k x) doesn't consume input, but is okay, we still return
                              ;;   in the consumed continuation
                              ;; - if (k x) doesn't consume input, but errors, we return the
                              ;;   error in the 'consumed-error' continuation
                              (run-parser (f x) s ret
                                :ret-val-empty (fn [x s e'] (ret-val-consumed ret x s (merge-error e e')))
                                :ret-err-empty (fn [e'] (ret-err-consumed ret (merge-error e e'))))))
        :ret-val-empty (fn [x s e]
                         (if (error-is-unknown e)
                           (run-parser (f x) s ret)
                           ;; - in these cases, (k x) can return as empty
                           (run-parser (f x) s ret
                             :ret-val-empty (fn [x s e'] (ret-val-empty ret x s (merge-error e e')))
                             :ret-err-empty (fn [e'] (ret-err-empty ret (merge-error e e'))))))))))

(defn fail [msg]
  (parser
    (fn [state ret]
      (ret-err-empty ret (new-error-message :msg/message msg (:pos state))))))

;; always fails without consuming any input
(declare zero)

(declare plus)

(declare label)

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
    (fn [state ret]
      (run-parser p state ret :ret-err-consumed (:ret-err-empty ret)))))

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

#_(defn >>
    [p1 p2]
    (bind p1 (fn const [_] p2)))

(defmacro with
  [[& bindings] & body]
  ;; TODO: validate macro arguments
  (let [[sym p] (take 2 bindings)]
    (if (= 2 (count bindings))
      `(bind ~p (fn [~sym] (let [p# ~@body]
                             ;; Allow return value directly in body
                             (cond-> p# (not (parser? p#)) (return)))))
      `(bind ~p (fn [~sym] (with ~(drop 2 bindings) ~@body))))))

(defn choice
  "Tries to apply the parsers in in order, until one of them succeeds. Returns
  the value of the succeeding parser."
  ([p1 p2]
   (parser
     (fn [state ret]
       (run-parser p1 state ret
         :ret-err-empty (fn [e] (run-parser p2 state ret
                                  :ret-val-empty (fn [x s e'] (ret-val-empty ret x s (merge-error e e')))
                                  :ret-err-empty (fn [e'] (ret-err-empty ret (merge-error e e')))))))))
  ([p1 p2 p3]
   (-> (choice p1 p2) (choice p3)))
  ([p1 p2 p3 & more]
   (reduce choice (list* p1 p2 p3 more))))

(defn option
  "Tries to apply parser `p`. If `p` fails without consuming input, it returns
  the value `x`, otherwise the value returned by `p`."
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
    (run-parser [_ state] (f state)))

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
    (loop [state state, res (run-parser p state)]
      (if (parser? res)
        (recur state (run-parser res state))
        res)))

#_(defn many
    "Parses p zero or more times; returns the result(s) in a vector. It stops when
    p fails, but this parser succeeds."
    [p]
    (Parser. (fn [state]
               (loop [res (parse-state* p state) vs (transient [])]
                 (if (state/state? res)
                   (recur (parse-state* p res) (conj! vs (:-value res)))
                   (state/fn-value (:state res) (persistent! vs)))))))

#_(defn parse
    [p input]
    (parse-state* p (state/->State (seq input) nil nil)))

(defn parse
  [p input]
  (run-parser p (State. (seq input) nil nil)))

(comment
  (def -input (seq "a"))
  (def -input (seq "abc123"))
  (def -input (seq "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))
  (def -input (seq "123"))

  (parse (return :ok) -input)
  (parse (bind (return :ok) #(return (str %))) -input)
  (parse (bind (fail :x1) (fn [x] (return :x2))) -input)
  (parse (with [x (return :x)
                y (return {:y x})]
           nil)
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

  (parser? letter?)
  (defn letter? [c] (Character/isLetter ^Character c))
  (defn letter? [c] (Character/isLetter ^Character c))
  (defn letter? [c] (= "a" c))
  (parse letter? -input)
  (parse (many letter?) -input)
  (seq -input)
  (run-parser letter? (state/->State -input nil nil))
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
