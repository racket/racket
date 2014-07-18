#;
(exn-pred #rx"could not convert type.*variable arity polymorphic type")
#lang racket/load

;; Check that contracts generation failure for mutually recursive type
;; is delayed until the exports with the type are actually used

(module a typed/racket
  (define-type Foo (All (x ...) (-> x ... x Bar)))
  (define-type Bar (U #f (Listof Foo)))

  (: f (-> Bar Void))
  (define (f x) (void))
  (provide f))

(require 'a)
f
