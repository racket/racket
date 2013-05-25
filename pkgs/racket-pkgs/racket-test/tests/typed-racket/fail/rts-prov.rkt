#;
(exn-pred exn:fail:syntax? #rx".*unbound identifier.*make-q.*")

#lang scheme/load

(module l scheme
  (define-struct q ())
  (provide (all-defined-out)))

(module m typed-scheme
  (require-typed-struct q () #:extra-constructor-name make-q 'l)
  (provide (all-defined-out)))

(module n typed-scheme
  (require 'm)
  (: f q)
  (define f (make-q)))
