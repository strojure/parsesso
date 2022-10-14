# parsesso

Parser combinators for Clojure(Script).

[![cljdoc badge](https://cljdoc.org/badge/com.github.strojure/parsesso)](https://cljdoc.org/d/com.github.strojure/parsesso)
[![Clojars Project](https://img.shields.io/clojars/v/com.github.strojure/parsesso.svg)](https://clojars.org/com.github.strojure/parsesso)

## Motivation

* Idiomatic and convenient API for parser combinators in Clojure and
  ClojureScript.

## Inspiration

* [haskell/parsec](https://github.com/haskell/parsec)
* [blancas/kern](https://github.com/blancas/kern)
* [youngnh/parsatron](https://github.com/youngnh/parsatron)
* [rm-hull/jasentaa](https://github.com/rm-hull/jasentaa)

## Cheat sheet

| Parsesso                 | Parsec<sup>[1],[2],[3]</sup>    | Kern<sup>[4]</sup>      | Parsatron<sup>[5]</sup> |
|--------------------------|---------------------------------|-------------------------|-------------------------|
| `result`                 | `return`                        | `return`                | `always`                |
| `fail`                   | `fail`                          | `fail`                  | `never`                 |
| `fail-unexpected`        | `unexpected`                    | `unexpected`            |                         |
| `expecting`              | `<?>`, `label`                  | `<?>`, `expect`         |                         |
| `bind`                   | `>>=`                           | `>>=`                   | `bind`                  |
| `for`                    | `do`                            | `bind`                  | `let->>`                |
| `after`                  | `>>`                            | `>>`                    | `>>`, `nxt`             |
| `value`                  | `fmap`                          | `<$>`                   |                         |
| `maybe`                  | `try`                           | `<:>`                   | `attempt`               |
| `look-ahead`             | `lookAhead`                     | `look-ahead`            | `lookahead`             |
| `not-followed-by`        | `notFollowedBy`                 | `not-followed-by`       |                         |
| `many0`                  | `many`                          | `many`                  | `many`                  |
| `many1`                  | `many1`                         | `many1`                 | `many1`                 |
| `skip0`                  | `skipMany`                      | `skip-many`             |                         |
| `skip1`                  | `skipMany1`                     | `skip-many1`            |                         |
| `token`                  | `token`, `satisfy`              | `satisfy`               | `token`                 |
| `token-not`              |                                 |                         |                         |
| `word`                   | `tokens`, `string`              | `token*`                | `string`                |
| `any-token`              | `anyToken`,`anyChar`            | `any-char`              | `any-char`              |
| `eof`                    | `eof`                           | `eof`                   | `eof`                   |
| `group`                  | `<*>`                           | `<*>`                   |                         |
| `choice`                 | <code><&#124;></code>, `choice` | <code><&#124;></code>   | `choice`                |
| `option`                 | `option`, `optional`            | `option`, `optional`    |                         |
| `between`                | `between`                       | `between`               | `between`               |
| `times`                  | `count`                         | `times`                 | `times`                 |
| `many-till`              | `manyTill`                      | `many-till`             |                         |
| `sep0`                   | `sepBy`                         | `sep-by`                |                         |
| `sep1`                   | `sepBy1`                        | `sep-by1`               |                         |
| `sep0-end`               | `endBy`                         | `end-by`                |                         |
| `sep1-end`               | `endBy1`                        | `end-by1`               |                         |
| `sep0-opt`               | `sepEndBy`                      | `sep-end-by`            |                         |
| `sep1-opt`               | `sepEndBy1`                     | `sep-end-by1`           |                         |
| `chain0-left`            | `chainl`                        | `chainl`                |                         |
| `chain1-left`            | `chainl1`                       | `chainl1`               |                         |
| `chain0-right`           | `chainr`                        | `chainr`                |                         |
| `chain1-right`           | `chainr1`                       | `chainr1`               |                         |
| `get-state`              | `getParserState`...             | input, pos, user state  |                         |
| `set-state`              | `setParserState`...             | input, pos, user state  |                         |
| `update-state`           | `updateParserState`...          | user state              |                         |
| `trace`                  | `parserTrace`, `parserTraced`   |                         |                         |
| `char/is`                | `char`, `oneOf`                 | `sym*`, `one-of*`       | `char`                  |
| `char/is-not`            | `noneOf`                        | `none-of*`              |                         |
| `char/regex`             |                                 |                         |                         |
| `char/upper?`            | `upper`                         | `upper` (unicode)       |                         |
| `char/lower?`            | `lower`                         | `lower` (unicode)       |                         |
| `char/letter?`           | `letter`                        | `letter` (unicode)      | `letter` (unicode)      |
| `char/number?`           | `digit`                         | `digit` (unicode)       | `digit` (unicode)       |
| `char/letter-or-number?` | `alphaNum`                      | `alpha-num` (unicode)   |                         |
| `char/white?`            | `space`                         | `white-space` (unicode) |                         |
| `char/newline`           | `endOfLine`                     | `new-line*`             |                         |
| `char/str*`              |                                 | `<+>`                   |                         |

[1]: https://github.com/haskell/parsec/blob/master/src/Text/Parsec/Prim.hs

[2]: https://github.com/haskell/parsec/blob/master/src/Text/Parsec/Combinator.hs

[3]: https://github.com/haskell/parsec/blob/master/src/Text/Parsec/Char.hs

[4]: https://github.com/blancas/kern/blob/master/src/main/clojure/blancas/kern/core.clj

[5]: https://github.com/youngnh/parsatron/blob/master/src/clj/the/parsatron.clj

## Examples

* [HoneySQL SELECT](test/demo/honeysql_select.clj)

## Performance

See some benchmarks [here](test/perf/bench.clj).
