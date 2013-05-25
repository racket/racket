#;
(exn-pred #rx"identifier bound to a structure type")
#lang typed/racket/base
(require/typed racket/async-channel
  [#:struct (async-channel +) ()])
