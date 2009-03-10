#lang scheme/load

(module l scheme
  (define-struct q ())
  (provide (all-defined-out)))

(module m typed-scheme
  (require-typed-struct q () 'l)
  (provide (all-defined-out)))

(module n typed-scheme
  (require 'm)
  (: f q)
  (define f (make-q)))
