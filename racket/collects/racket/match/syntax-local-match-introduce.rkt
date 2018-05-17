#lang racket/base

(provide syntax-local-match-introduce)

(define (syntax-local-match-introduce x)
  (unless (syntax? x)
    (raise-argument-error 'syntax-local-match-introduce "syntax?" x))
  (syntax-local-introduce x))
