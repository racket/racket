#lang racket/base

(provide compiler-security-guard
         pick-security-guard
         with-compiler-security-guard)

(define compiler-security-guard (make-parameter #f))

(define (pick-security-guard)
  (or (compiler-security-guard)
      (current-security-guard)))

(define-syntax-rule (with-compiler-security-guard expr)
  (parameterize ([current-security-guard (pick-security-guard)]) 
    expr))

