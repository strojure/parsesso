(ns strojure.parsessor.core
  (:require [strojure.parsessor.impl.pos :as pos])
  #?(:clj (:import (clojure.lang IFn))))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

#?(:clj (deftype Parser [parser-fn]
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
                    1 e1, -1 e2, (ParseError. pos1 (reduce conj m1 m2)))))))

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

#?(:clj  (deftype Continue [f] IFn (invoke [_] (.invoke ^IFn f)))
   :cljs (deftype Continue [f] IFn (-invoke [_] (-invoke f))))

(defn- call
  ([p state ret]
   (Continue. (fn continue [] (p state ret))))
  ([p state ret k1 f1]
   (call p state (-> ret (assoc k1 f1))))
  ([p state ret k1 f1 k2 f2]
   (call p state (-> ret (assoc k1 f1) (assoc k2 f2))))
  ([p state ret k1 f1 k2 f2 k3 f3]
   (call p state (-> ret (assoc k1 f1) (assoc k2 f2) (assoc k3 f3))))
  ([p state ret k1 f1 k2 f2 k3 f3 k4 f4]
   (call p state (-> ret (assoc k1 f1) (assoc k2 f2) (assoc k3 f3) (assoc k4 f4)))))

#_(defn mkpt
    "k is (fn [state] reply)"
    [f]
    (parser
      (fn [state ret]
        (let [reply (f state)]
          (if (:consumed reply)
            (if (value-reply? reply)
              (ret-val-consumed ret (:value reply) (:state reply) (:error reply))
              (ret-err-consumed ret (:error reply)))
            (if (value-reply? reply)
              (ret-val-empty ret (:value reply) (:state reply) (:error reply))
              (ret-err-empty ret (:error reply))))))))

#_(defn fmap-reply
    [f reply]
    (cond-> reply
      (value-reply? reply) (update :value f)))

#_(defn parsec-map
    [f p]
    (parser
      (fn [state ret]
        (call p state ret
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
      (call p state ret
        :ret-val-consumed (fn [x s e]
                            (if (error-is-unknown e)
                              (call (f x) s ret)
                              ;; - if (k x) consumes, those go straight up
                              ;; - if (k x) doesn't consume input, but is okay, we still return
                              ;;   in the consumed continuation
                              ;; - if (k x) doesn't consume input, but errors, we return the
                              ;;   error in the 'consumed-error' continuation
                              (call (f x) s ret
                                :ret-val-empty (fn [x s e'] (ret-val-consumed ret x s (merge-error e e')))
                                :ret-err-empty (fn [e'] (ret-err-consumed ret (merge-error e e'))))))
        :ret-val-empty (fn [x s e]
                         (if (error-is-unknown e)
                           (call (f x) s ret)
                           ;; - in these cases, (k x) can return as empty
                           (call (f x) s ret
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
      (call p state ret :ret-err-consumed (:ret-err-empty ret)))))

(defn look-ahead
  ;; TODO: Update reference to `try`.
  "Parses `p` without consuming any input. If `p` fails and consumes some input,
  so does `look-ahead`. Combine with `try` if this is undesirable."
  [p]
  (parser
    (fn [state ret]
      (let [ret-val (fn [x _ _] (ret-val-empty ret x state (new-error-unknown (:pos state))))]
        (call p state ret :ret-val-consumed ret-val :ret-val-empty ret-val)))))

(declare token, token-prim, token-prim-ex)

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
     (fn [state ret]
       (if-let [input (seq (:input state))]
         (let [t (first input)]
           (if (pred t)
             (let [pos (:pos state)
                   new-input (rest input)
                   new-pos (pos/next-pos pos t new-input)
                   new-state (->State new-input new-pos (cond->> (:user state)
                                                          user-fn (user-fn pos t new-input)))]
               (ret-val-consumed ret t new-state (new-error-unknown new-pos)))
             (ret-err-empty ret (unexpect-error (delay (msg-fn t)) (:pos state)))))
         (ret-err-empty ret (unexpect-error "" (:pos state))))))))

(comment
  (def -input "abc")
  (parse (token #(= \a %)) -input)
  (parse (token #(= \b %)) -input)
  (parse (choice (token #(= \a %)) (token #(= \b %))) -input)
  (parse (choice (token #(= \b %)) (token #(= \a %))) -input)
  )

(defn- many-accum
  "Accumulates values using reducing function `rf`."
  [p rf]
  (parser
    (fn [state ret]
      (let [error (fn [& _] (throw (ex-info "Combinator 'many' is applied to a parser that accepts an empty string." {})))
            walk (fn walk [xs x s _e]
                   (let [xs' (rf xs x)]
                     (call p s ret :ret-val-consumed (partial walk xs')
                                   :ret-err-empty (fn [e] (ret-val-consumed ret (rf xs') s e))
                                   :ret-val-empty error)))
            xs (rf)]
        (call p state ret :ret-val-consumed (partial walk xs)
                          :ret-err-empty (fn [e] (ret-val-consumed ret (rf xs) state e))
                          :ret-val-empty error)))))

(defn many
  "Applies the parser `p` zero or more times. Returns a vector of the returned values or `p`."
  [p]
  (many-accum p (completing conj! persistent!)))

(comment
  (def -input "")
  (def -input (seq "abc123"))
  (def -input (seq "123"))
  (def -input (repeat 10000 \a))
  (parse (many (token #(Character/isLetter ^char %))) -input)
  )

(declare skip-many)

(defn skip-many
  "Applies the parser `p` zero or more times, skipping its result."
  [p]
  (many-accum p (fn ([]) ([_]) ([_ _]))))

#_(defn skip-many
    "Applies the parser `p` zero or more times, skipping its result."
    [p]
    (many-accum p nil))

(comment
  (def -input "")
  (def -input (seq "abc123"))
  (def -input (seq "123"))
  (def -input (repeat 10000 \a))
  (parse (skip-many (token #(Character/isLetter ^char %))) -input)
  )

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
       (call p1 state ret
         :ret-err-empty (fn [e] (call p2 state ret
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

(defn parse
  [p input]
  ;; TODO: Initialize source pos
  (loop [reply (p (State. (seq input) 1 nil) (init-reply-api))]
    (if (instance? Continue reply)
      (recur (reply))
      reply)))

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
