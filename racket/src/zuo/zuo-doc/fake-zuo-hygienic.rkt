#lang racket/base

(define-syntax-rule (define-fake id ...)
  (begin
    (provide id ...)
    (define id 'id) ...))

(define-syntax-rule (intro-define-fake)
  (define-fake
    identifier?
    syntax-e
    syntax->datum
    datum->syntax
    bound-identifier=?))

(intro-define-fake)
