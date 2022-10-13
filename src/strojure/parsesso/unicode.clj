(ns strojure.parsesso.unicode
  "Unicode char parsers using `java.lang.Character`. Clojure only."
  (:require [strojure.parsesso.parser :as p]))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(def lower?
  "Parser and predicate for the lower-case letter character according to
  `Character/isLowerCase`."
  (p/token #(Character/isLowerCase ^char %)
           "lower-case letter"))

(def upper?
  "Parser and predicate for the upper-case letter character according to
  `Character/isUpperCase`."
  (p/token #(Character/isUpperCase ^char %)
           "upper-case letter"))

(def title?
  "Parser and predicate for the title-case letter character according to
  `Character/isTitleCase`."
  (p/token #(Character/isTitleCase ^char %)
           "title-case letter"))

(def digit?
  "Parser and predicate for the digit character according to
  `Character/isDigit`."
  (p/token #(Character/isDigit ^char %)
           "digit"))

(def defined?
  "Parser and predicate for the character defined in Unicode, according to
  `Character/isDefined`."
  (p/token #(Character/isDefined ^char %)
           "unicode defined character"))

(def letter?
  "Parser and predicate for the letter character according to
  `Character/isLetter`."
  (p/token #(Character/isLetter ^char %)
           "letter"))

(def letter-or-digit?
  "Parser and predicate for the letter or digit character according to
  `Character/isLetterOrDigit`."
  (p/token #(Character/isLetterOrDigit ^char %)
           "letter or digit"))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(def space?
  "Parser and predicate for the Unicode space character according to
  `Character/isSpaceChar`."
  (p/token #(Character/isSpaceChar ^char %)
           "space character"))

(def white?
  "Parser and predicate for the white space character according to
  `Character/isWhitespace`."
  (p/token #(Character/isWhitespace ^char %)
           "whitespace character"))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
