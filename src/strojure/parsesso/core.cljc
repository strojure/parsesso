(ns strojure.parsesso.core
  (:require [strojure.parsesso.impl.pos :as pos]
            [strojure.parsesso.impl.reply :as r])
  (:import (clojure.lang IFn)
           (strojure.parsesso.impl.reply Context)))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defprotocol ICont
  (run-cont [c]))

(deftype Continue [f]
  ICont (run-cont [_] (f)))

(defprotocol IParser
  (continue [p state context]
    "Applies parser function in continuation."))

(extend-type Context
  IParser
  (continue [context p state]
    ;; No reuse `(continue p state context)` for performance.
    (Continue. (fn [] (p state context)))))

#?(:clj
   (deftype Parser [f]
     IFn
     (invoke [_p state] (f state (r/new-context)))
     (invoke [_p state context] (f state context))
     IParser
     (continue [_p state context] (Continue. (fn [] (f state context))))))

(defn parser
  "Wraps function `(fn [state context])` in the instance of `Parser`."
  [f]
  (Parser. f))

(defn parser? [p] (instance? Parser p))

(defrecord State [input pos user])

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defrecord ParseError [pos messages])

(defn new-error-unknown [pos] (ParseError. pos nil))
(defn new-error-message [typ msg pos] (ParseError. pos [[typ msg]]))

(defn error-is-unknown [error] (nil? (:messages error)))

(defn merge-error [e1 e2]
  (let [m1 (:messages e1), m2 (:messages e2)]
    (cond (and m1 (nil? m2)) e1
          (and m2 (nil? m1)) e2
          :else (let [pos1 (:pos e1)]
                  (case (compare pos1 (:pos e2))
                    1 e1, -1 e2, (ParseError. pos1 (reduce conj m1 m2)))))))

(defn unknown-error [state]
  (new-error-unknown (:pos state)))

(defn add-error-message
  [err typ msg]
  (update err :messages (fnil conj []) [typ msg]))

(defn set-error-message
  [err typ msg]
  ;; TODO: filter duplicates
  (update err :messages (fnil conj []) [typ msg]))

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
      (r/e-err context (new-error-message :msg/un-expect msg (:pos state))))))

(defn value
  [x]
  (parser
    (fn [state context]
      (r/e-ok context x state (unknown-error state)))))

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
                                          (r/c-ok context x s (if (error-is-unknown e) ee (merge-error e ee)))))
                            (r/set-e-err (fn [ee]
                                           (r/c-err context (if (error-is-unknown e) ee (merge-error e ee)))))
                            (continue (f x) s))))
          (r/set-e-ok (fn [x s e]
                        (if (error-is-unknown e)
                          (continue context (f x) s)
                          ;; - in these cases, (f x) can return as empty
                          (-> context
                              (r/set-e-ok (fn [x s ee]
                                            (r/e-ok context x s (merge-error e ee))))
                              (r/set-e-err (fn [ee]
                                             (r/e-err context (merge-error e ee))))
                              (continue (f x) s)))))
          (continue p state)))))

(defn error
  "Always fails without consuming any input."
  [msg]
  (parser
    (fn [state context]
      (r/e-err context (new-error-message :msg/message msg (:pos state))))))

;; TODO: remove zero?
(declare zero)

;; TODO: remove plus?
(declare plus)

;; TODO: Remove labels from API?
(defn labels
  [p messages]
  (parser
    (fn [state context]
      (letfn [(set-expect-errors [e [msg & more :as messages]]
                (cond
                  more, (->> messages (reduce (fn [e msg] (add-error-message e :msg/expect msg)) e))
                  msg,, (set-error-message e :msg/expect msg)
                  :else (set-error-message e :msg/expect "")))]
        (-> context
            (r/set-e-ok (fn [x s e]
                          (r/e-ok context x s (cond-> e (not (error-is-unknown e))
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

(defn trim
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

(comment
  (parse (trim (>> (token #{\x}) (token #{\x}))) "xy")
  (parse (trim (>> (token #{\x}) (token #{\x}))) "xx")
  )

(defn look-ahead
  ;; TODO: Update reference to `try`.
  "Parses `p` without consuming any input. If `p` fails and consumes some input,
  so does `look-ahead`. Combine with `try` if this is undesirable."
  [p]
  (parser
    (fn [state context]
      (let [e-ok (fn [x _ _] (r/e-ok context x state (new-error-unknown (:pos state))))]
        (-> context
            (r/set-c-ok e-ok)
            (r/set-e-ok e-ok)
            (continue p state))))))

(defn- unexpect-error
  [msg pos]
  (new-error-message :msg/sys-unexpect msg pos))

(def token-msg (partial str "token: "))

;; TODO: optimal order of arguments?
(defn token
  "Returns the parser which accepts a token when `(pred token)` returns logical
  true. The token can be shown in error message using `(msg-fn token)`."
  ([pred] (token pred token-msg pos/next-pos nil))
  ([pred, msg-fn] (token pred msg-fn pos/next-pos nil))
  ;; TODO: split to two versions for get-next-user like in haskell (for performance?)
  ([pred, msg-fn, pos-fn] (token pred msg-fn pos-fn nil))
  ([pred, msg-fn, pos-fn, user-fn]
   (parser
     (fn [state context]
       (if-let [input (seq (:input state))]
         (let [tok (first input)]
           (if (pred tok)
             (let [pos (:pos state)
                   new-input (rest input)
                   new-pos (pos-fn pos tok new-input)
                   new-state (->State new-input new-pos (cond->> (:user state)
                                                          user-fn (user-fn pos tok new-input)))]
               (r/c-ok context tok new-state (new-error-unknown new-pos)))
             (r/e-err context (unexpect-error (delay (msg-fn tok)) (:pos state)))))
         (r/e-err context (unexpect-error "" (:pos state))))))))

(comment
  (def -input "abc")
  (parse (token #(= \a %)) -input)
  (parse (token #(= \b %)) -input)
  (parse (choice (token #(= \a %)) (token #(= \b %))) -input)
  (parse (choice (token #(= \b %)) (token #(= \a %))) -input)
  )

(defn- throw-empty-input
  [sym]
  (fn [_ _ _]
    (throw (ex-info (str "Combinator '" sym "' is applied to a parser that accepts an empty input.") {}))))

;; TODO: return nil or [] for empty result?
(defn take*
  "Applies the parser `p` zero or more times. Returns a vector of the returned
  values or `p`. Optional `init` is a collection to add values to."
  ([p] (take* p []))
  ([p init]
   (parser
     (fn [state context]
       (let [context (-> context (r/set-e-ok (throw-empty-input 'take*)))
             walk (fn walk [xs x s _e]
                    (let [xs (conj! xs x)]
                      (-> context
                          (r/set-c-ok (partial walk xs))
                          (r/set-e-err (fn [e]
                                         (r/c-ok context (persistent! xs) s e)))
                          (continue p s))))]
         (-> context
             (r/set-c-ok (partial walk (transient init)))
             (r/set-e-err (partial r/c-ok context init state))
             (continue p state)))))))

(comment
  (def -input "")
  (def -input (seq "abc123"))
  (def -input (seq "123"))
  (def -input (repeat 10000 \a))
  (parse (take* (token #(Character/isLetter ^char %))) -input)
  )

(defn skip*
  "Applies the parser `p` zero or more times, skipping its result."
  [p]
  (parser
    (fn [state context]
      (let [context (-> context (r/set-e-ok (throw-empty-input 'skip*)))
            walk (fn walk [_x s _e]
                   (-> context
                       (r/set-c-ok walk)
                       (r/set-e-err (partial r/c-ok context nil s))
                       (continue p s)))]
        (-> context
            (r/set-c-ok walk)
            (r/set-e-err (partial r/c-ok context nil state))
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

;; TODO: consider >> in api
(defn >>
  [p1 p2]
  (bind* p1 (fn const [_] p2)))

(defmacro bind
  [[& bindings] & body]
  ;; TODO: validate macro arguments
  (let [[sym p] (take 2 bindings)]
    (if (= 2 (count bindings))
      `(bind* ~p (fn [~sym] (let [p# ~@body]
                              ;; Allow return value directly in body
                              (cond-> p# (not (parser? p#)) (value)))))
      `(bind* ~p (fn [~sym] (bind ~(drop 2 bindings) ~@body))))))

(defn alt
  "Tries to apply the parsers in order, until one of them succeeds. Returns the
  value of the succeeding parser."
  ([p1 p2]
   (parser
     (fn [state context]
       (-> context
           (r/set-e-err (fn [e]
                          (-> context
                              (r/set-e-ok (fn [x s ee]
                                            (r/e-ok context x s (merge-error e ee))))
                              (r/set-e-err (fn [ee]
                                             (r/e-err context (merge-error e ee))))
                              (continue p2 state))))
           (continue p1 state)))))
  ([p1 p2 p3]
   (-> (alt p1 p2) (alt p3)))
  ([p1 p2 p3 & more]
   (reduce alt (list* p1 p2 p3 more))))

(defn option
  "Tries to apply parser `p`. If `p` fails without consuming input, it returns
  the value `x`, otherwise the value returned by `p`."
  [x p]
  (alt p (value x)))

(defn optional
  "Tries to apply parser `p`. It will parse `p` or nothing. It only fails if `p`
  fails after consuming input. It discards the result of `p`."
  [p]
  (alt (bind [_ p] (value nil))
       (value nil)))

(defn between
  "Parses `open`, followed by `p` and `close`. Returns the value returned by `p`."
  ([p around] (between p around around))
  ([p open close]
   (bind [_ open, x p, _ close]
     (value x))))

(defn skip+
  "Applies the parser `p` /one/ or more times, skipping its result."
  [p]
  (bind [_ p]
    (skip* p)))

(defn take+
  "Applies the parser `p` /one/ or more times. Returns a list of the returned
  values of `p`."
  [p]
  (bind [x p]
    (take* p [x])))

;; TODO: argument order
;; TODO: Check if it should be consumed or not if n > length.
;; TODO: Rewrite similar to haskell?
(defn take-count
  "Parses `n` occurrences of `p`. If `n` is smaller or equal to zero, the parser
  equals to `(value nil)`. Returns a list of `n` values returned by `p`."
  [n p]
  (if (pos? n) (bind [x p, xs (take-count (dec n) p)]
                 (value (cons x xs)))
               (value nil)))

(comment
  (parse (take-count 2 (token #{\x})) "xxxyyy")
  (parse (take-count 4 (token #{\x})) "xxxyyy")
  )

(declare sep-by+)

(defn sep-by*
  "Parses /zero/ or more occurrences of `p`, separated by `sep`. Returns a
  vector of values returned by `p`."
  [p sep]
  (alt (sep-by+ p sep) (value nil)))

(defn sep-by+
  "Parses /one/ or more occurrences of `p`, separated by `sep`. Returns a vector
  of values returned by `p`."
  [p sep]
  (bind [x p]
    (take* (>> sep p) [x])))

(defn sep-by-end*
  "Parses /zero/ or more occurrences of `p`, separated and ended by `sep`.
  Returns a list of values returned by `p`."
  [p sep]
  (take* (bind [x p, _ sep] (value x))))

(defn sep-by-end+
  "Parses /one/ or more occurrences of `p`, separated and ended by `sep`.
  Returns a list of values returned by `p`."
  [p sep]
  (take+ (bind [x p, _ sep] (value x))))

(declare sep-by-end?+)

(defn sep-by-end?*
  "Parses /zero/ or more occurrences of `p`, separated and optionally ended by
  `sep`. Returns a list of values returned by `p`."
  [p sep]
  (alt (sep-by-end?+ p sep)
       (value nil)))

(defn sep-by-end?+
  "Parses /one/ or more occurrences of `p`, separated and optionally ended by
  `sep`. Returns a vector of values returned by `p`."
  [p sep]
  (bind [x p]
    (alt (bind [_ sep, xs (sep-by-end?* p sep)]
           ;; TODO: cons?
           (value (cons x xs)))
         (value [x]))))

(declare chain-right+)

(defn chain-right*
  "Parses /zero/ or more occurrences of `p`, separated by `op`. Returns a value
  obtained by a /right/ associative application of all functions returned by
  `op` to the values returned by `p`. If there are no occurrences of `p`, the
  value `x` is returned."
  [p op x]
  (alt (chain-right+ p op)
       (value x)))

(defn chain-right+
  "Parses /one/ or more occurrences of `p`, separated by `op`. Returns a value
  obtained by a /right/ associative application of all functions returned by
  `op` to the values returned by `p`."
  [p op]
  (letfn [(scan [] (bind [x p] (more x)))
          (more [x] (alt (bind [f op, y (scan)] (value (f x y)))
                         (value x)))]
    (scan)))

(declare chain-left+)

(defn chain-left*
  "Parses /zero/ or more occurrences of `p`, separated by `op`. Returns a value
  obtained by a /left/ associative application of all functions returned by `op`
  to the values returned by `p`. If there are zero occurrences of `p`, the value
  `x` is returned."
  [p op x]
  (alt (chain-left+ p op)
       (value x)))

(defn chain-left+
  "Parses /one/ or more occurrences of `p`, separated by `op` Returns a value
  obtained by a /left/ associative application of all functions returned by `op`
  to the values returned by `p`. This parser can for example be used to
  eliminate left recursion which typically occurs in expression grammars."
  [p op]
  (letfn [(more [x] (alt (bind [f op, y p] (more (f x y)))
                         (value x)))]
    (bind [x p]
      (value (more x)))))

;;; Tricky combinators

(def any-token
  "Accepts any kind of token. It is for example used to implement 'eof'. Returns
  the accepted token."
  (token any? token-msg (fn [pos _ _] pos)))

(defn not-followed-by
  "Only succeeds when parser `p` fails. This parser does not consume any input.
  This parser can be used to implement the 'longest match' rule. For example,
  when recognizing keywords (for example `let`), we want to make sure that a
  keyword is not followed by a legal identifier character, in which case the
  keyword is actually an identifier (for example `lets`)."
  [p]
  (trim (alt (bind [c (trim p)] (unexpected (delay (str c))))
             (value nil))))

(def eof
  "This parser only succeeds at the end of the input. This is not a primitive
  parser but it is defined using 'notFollowedBy'."
  ;; TODO: Implement using direct access to input for performance?
  (-> (not-followed-by any-token)
      (label "end of input")))

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
  (loop [reply (p (State. (seq input) 1 nil))]
    (if (instance? Continue reply)
      (recur (run-cont reply))
      reply)))

(comment
  (def -input (seq "a"))
  (def -input (seq "abc123"))
  (def -input (seq "a1b2c3"))
  (def -input (seq "a1b2c"))
  (def -input (seq "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))
  (def -input (seq "123"))
  (def -input (repeat 10000 \a))

  (-> (take* (token #(Character/isLetter ^char %)))
      (parse -input))
  (-> (take+ (token #(Character/isLetter ^char %)))
      (parse -input))

  (-> (>> (token #(Character/isLetter ^char %))
          (token #(Character/isDigit ^char %)))
      (parse -input))
  (-> (sep-by* (token #(Character/isLetter ^char %))
               (token #(Character/isDigit ^char %)))
      (parse -input))
  (-> (sep-by+ (token #(Character/isLetter ^char %))
               (token #(Character/isDigit ^char %)))
      (parse -input))
  (-> (sep-by-end?* (token #(Character/isLetter ^char %))
                    (token #(Character/isDigit ^char %)))
      (parse -input))
  (-> (sep-by-end?+ (token #(Character/isLetter ^char %))
                    (token #(Character/isDigit ^char %)))
      (parse -input))

  (parse (optional (error :ok)) nil)

  (def -input (seq "[]"))
  (def -input (seq "[abc]"))
  (def -input (seq "[abc123]"))
  (-> (take* (token #(Character/isLetter ^char %)))
      (between (token #(= \[ %)) (token #(= \] %)))
      (parse -input))

  (parse (value :ok) -input)
  (parse (bind* (value :ok) #(value (str %))) -input)
  (parse (bind* (error :x1) (fn [x] (value :x2))) -input)
  (parse (bind [x (value :x)
                y (value {:y x})]
           y)
         -input)

  (parse (error "oops") -input)
  (parse (alt (value :ok) (error "oops")) -input)
  (parse (alt (error "oops") (value :ok)) -input)
  (parse (alt (error "oops") (error "oops2") (value :ok)) -input)
  (parse (alt (error "oops") (error "oops2")) -input)
  (def -p (alt (error "oops") (value :ok)))
  (def -p (alt (error "oops") (error "oops2") (value :ok)))
  (def -p (alt (value :ok) (error "oops") (error "oops2")))
  (parse -p -input)
  (parse (trim (error "oops")) -input)
  (parse (trim (value :ok)) -input)

  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
