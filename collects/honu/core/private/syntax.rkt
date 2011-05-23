#lang scheme

(provide (all-defined-out))

(define honu-scheme-syntax 'honu-scheme-syntax)

(define (raw-scheme? stx)
  (syntax-property stx honu-scheme-syntax))

(define (apply-scheme-syntax stx)
  (syntax-property stx honu-scheme-syntax #t))
