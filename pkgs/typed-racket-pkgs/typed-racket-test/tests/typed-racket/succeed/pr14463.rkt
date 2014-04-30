#lang typed/racket/no-check

;; Test for PR 14463. Check that require/typed with #:opaque works
;; in no-check mode

(module untyped racket
  (provide f thing?)
  (define (f x) x)
  (define (thing? x) #t))

(require/typed 'untyped
               [#:opaque Thing thing?]
               [f (Thing -> Thing)])

(thing? 3)
