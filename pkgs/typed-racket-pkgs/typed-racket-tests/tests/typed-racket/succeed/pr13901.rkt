#lang racket/load

;; This test ensures that require/typed/provide works for
;; all of the clauses that are documented for require/typed

(require rackunit)

(module a typed/racket
  (require/typed/provide
   racket/base
   [#:opaque Evt evt?]
   [never-evt Evt]))
(require 'a)

(check-false (evt? 5))
(check-true (evt? never-evt))

(module b-provider racket
  (struct foo (value))
  (provide (struct-out foo)))

(module b typed/racket
  (require/typed/provide
   'b-provider
   [#:struct foo ([value : Exact-Nonnegative-Integer])]))
(require 'b)

(module c-provider racket
  (struct bar (x))
  (struct baz bar (y z))
  (provide (struct-out bar)
           (struct-out baz)))

(module c typed/racket
  (require/typed/provide
   'c-provider
   [#:struct bar ([x : Integer])]
   [#:struct (baz bar) ([y : String] [z : String])]))
(require 'c)

