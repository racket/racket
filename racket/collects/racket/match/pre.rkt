#lang racket/base

(require "runtime.rkt" (for-syntax racket/base racket/lazy-require))

(begin-for-syntax
  (lazy-require [racket/match/gen-match (go/one)])
  (lazy-require [racket/match/parse (parse)]))

(define-syntax (match stx)
  (syntax-case stx ()
    [(_ arg clauses ...)
     (go/one parse stx #'arg #'(clauses ...))]))

(provide match)
