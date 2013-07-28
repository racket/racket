#lang scheme/load


(module m typed-scheme
  (define-typed-struct A ())
  (define-typed-struct (x A) ([y : Number] [z : Boolean]))
  (: foo (x -> Number))
  (define (foo z) 1)
  (provide (all-defined-out)))

(module n scheme
  (require (prefix-in m: 'm))
  (m:foo (m:make-x 1 #f))
  m:x?)

(require 'n)
