#lang racket/base

;; Tests for keyword ordering issues with mandatory keyword
;; arguments

(module a racket
  (define (f a #:w [w "w"] #:x x #:y [y "y"]) x)
  (provide f))

(module b typed-racket/base-env/extra-env-lang
  (require (submod ".." a))
  (type-environment
    [f (->key -Symbol
              ;; this alphabetic ordering of keywords should
              ;; be preserved in the kw type conversion for applications,
              ;; rather than separating mandatory/optional as for lambdas
              #:w -String #f
              #:x -Symbol #t
              #:y -String #f
              -Symbol)]))

(module c typed/racket
  (require (submod ".." b))
  (f 'a #:x 'x))

(require 'c)
