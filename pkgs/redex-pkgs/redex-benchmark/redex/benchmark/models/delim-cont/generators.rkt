#lang racket/base

(require (except-in redex/benchmark/models/delim-cont/delim-cont let)
         (only-in redex/private/generate-term pick-an-index)
         redex/reduction-semantics
         racket/bool)

(provide (all-defined-out))

(module+ adhoc-mod
  (provide generate get-generator type)
  (define (get-generator) generate)
  (define type 'grammar)
  (define (generate)
    (generate-term abort-lang e 4)))

(module+ enum-mod
  (provide generate get-generator type)
  (define (get-generator) generate)
  (define type 'enum)
  (define (generate [p-value 0.125])
    (generate-term abort-lang e #:i-th (pick-an-index p-value))))

(module+ ordered-mod
  (provide generate get-generator type)
  (define (get-generator)
    (let ([index 0])
      (λ () (begin0
              (generate index)
              (set! index (add1 index))))))
  (define type 'ordered)
  (define (generate [index 0])
    (generate-term abort-lang e #:i-th index)))

(module+ check-mod
  (provide check)
  (define (check term)
  (or (not term)
      (implies (type-check term)
               (soundness-holds? term)))))

