# parsesso

Parser combinators for Clojure(Script).

[![Clojars Project](https://img.shields.io/clojars/v/com.github.strojure/parsesso.svg)](https://clojars.org/com.github.strojure/parsesso)
[![cljdoc badge](https://cljdoc.org/badge/com.github.strojure/parsesso)](https://cljdoc.org/d/com.github.strojure/parsesso)

## Motivation

* Idiomatic and convenient API for parser combinators in Clojure and
  ClojureScript.

## Inspiration

* [haskell/parsec](https://github.com/haskell/parsec)
* [blancas/kern](https://github.com/blancas/kern)
* [youngnh/parsatron](https://github.com/youngnh/parsatron)
* [rm-hull/jasentaa](https://github.com/rm-hull/jasentaa)

## Cheat sheet

| Parsec                          | Parsesso          | Kern                  | Parsatron          |
|---------------------------------|-------------------|-----------------------|--------------------|
| `return`                        | `result`          | `return`              | `always`           |
| `fail`                          | `fail`            | `fail`                | `never`            |
| `label`, `<?>`                  | `expecting`       | `<?>`                 |                    |
| `do`                            | `bind-let`        | `bind`                | `let->>`           |
| `>>`                            | `after`           | `>>`                  | `>>`, `nxt`        |
| `fmap`                          | `using`           | `<$>`                 |                    |
| `try`                           | `maybe`           | `<:>`                 | `attempt`          |
| `lookAhead`                     | `look-ahead`      | `look-ahead`          | `lookahead`        |
| `notFollowedBy`                 | `not-followed-by` | `not-followed-by`     |                    |
| `many`                          | `many0`           | `many`                | `many`             |
| `many1`                         | `many1`           | `many1`               | `many1`            |
| `skipMany`                      | `skip0`           | `skip-many`           |                    |
| `skipMany1`                     | `skip1`           | `skip-many1`          |                    |
| `token`, `satisfy`              | `token`           | `satisfy`             | `token`            |
| `tokens`, `string`              | `word`            | `token*`              | `string`           |
| `<*>`                           | `each`,`tuple`    | `<*>`                 |                    |
| <code><&#124;></code>, `choice` | `choice`          | <code><&#124;></code> | `either`, `choice` |
| `option`                        | `option`          | `option`              |                    |
| `optional`                      | `option`          | `optional`            |                    |
| `count`                         | `times`           | `times`               | `times`            |

## Performance

See some benchmarks [here](test/perf/bench.clj).
