#lang racket/base
(provide (all-defined-out))

(define-values (prop:pattern-expander pattern-expander? get-proc-getter)
  (make-struct-type-property 'pattern-expander))

(define (pattern-expander-proc pat-expander)
  (define get-proc (get-proc-getter pat-expander))
  (get-proc pat-expander))

(define current-syntax-parse-pattern-introducer
  (make-parameter
   (lambda (stx)
     (error 'syntax-local-syntax-parse-pattern-introduce "not expanding syntax-parse pattern"))))

(define (syntax-local-syntax-parse-pattern-introduce stx)
  ((current-syntax-parse-pattern-introducer) stx))
