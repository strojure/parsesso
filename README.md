# parsesso

Parser combinators for Clojure(Script).

[![Clojars Project](https://img.shields.io/clojars/v/com.github.strojure/parsesso.svg)](https://clojars.org/com.github.strojure/parsesso)

## Motivation

* Idiomatic and convenient API for parser combinators in Clojure and
  ClojureScript.

## Inspiration

* [haskell/parsec](https://github.com/haskell/parsec)
* [blancas/kern](https://github.com/blancas/kern)
* [youngnh/parsatron](https://github.com/youngnh/parsatron)
* [rm-hull/jasentaa](https://github.com/rm-hull/jasentaa)

## Correspondence of function names

| Parsec             | Parsesso          | Kern              | Parsatron          |
|--------------------|-------------------|-------------------|--------------------|
| `return`           | `result`          | `return`          | `always`           |
| `fail`             | `fail`            | `fail`            | `never`            |
| `label`, `<?>`     | `expecting`       | `<?>`             |                    |
| `do`               | `bind-let`        | `bind`            | `let->>`           |
| `>>`               | `after`           | `>>`              | `>>`, `nxt`        |
| `fmap`             | `with`            | `<?>`             |                    |
| `try`              | `maybe`           | `<:>`             | `attempt`          |
| `lookAhead`        | `look-ahead`      | `look-ahead`      | `lookahead`        |
| `notFollowedBy`    | `not-followed-by` | `not-followed-by` |                    |
| `many`             | `many-zero`       | `many`            | `many`             |
| `many1`            | `many-some`       | `many1`           | `many1`            |
| `skipMany`         | `skip-zero`       | `skip-many`       |                    |
| `skipMany1`        | `skip-some`       | `skip-many1`      |                    |
| `token`, `satisfy` | `token`           | `satisfy`         | `token`            |
| `tokens`, `string` | `word`            | `token*`          | `string`           |
| `<*>`              | `each`            | `<*>`             |                    |
| `<!>`, `choice`    | `choice`          | `<!>`             | `either`, `choice` |
| `option`           | `optional`        | `optional`        |                    |
| `optional`         | `optional`        | `option`          |                    |
| `count`            | `times`           | `times`           | `times`            |

## Performance

See some benchmarks [here](test/perf/bench.clj).
