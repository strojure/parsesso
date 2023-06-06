# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## `1.2.2-SNAPSHOT`

Release date `UNRELEASED`

- (fix): allow destructuring in `p/for` [#8]

[#8]: https://github.com/strojure/parsesso/issues/8

## `1.2.1+292`

Release date `2023-05-28`

- (docs): fix :arglists of `parser/parse` [#7]

[#7]: https://github.com/strojure/parsesso/issues/7

## `1.2.0+287`

Release date `2023-05-25`

- (feat pos): allow to specify initial line/col for :text pos
- (fix): cannot pass custom `InputPos` [#6]

[#6]: https://github.com/strojure/parsesso/issues/6

## `1.1.2-283`

Release date `2023-05-17`

- (fix): `expecting` adds a message instead of replacing [#5]

[#5]: https://github.com/strojure/parsesso/issues/5

## `1.1.1-274`

Release date `2023-03-08`

- (chore project) Implement `cljs-test` lein alias.
- (fix cljs) `parser/update-state` for nil :input.
- (chore) Change license to Unlicense.

## `1.1.0-258`

Release date `2023-03-04`

- feat: Make code compatible with `bb` and other platforms.
- build: Add CI config to run lein test + bb test:bb.

## `1.0.253`

Release date `2023-03-04`
