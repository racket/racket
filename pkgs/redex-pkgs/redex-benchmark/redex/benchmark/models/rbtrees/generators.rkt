#lang racket/base

(require redex/examples/rbtrees
         (only-in redex/private/generate-term pick-an-index)
         redex/reduction-semantics
         racket/match
         racket/bool)

(provide (all-defined-out))

(module+ adhoc-mod
  (provide generate get-generator type)
  (define (get-generator) generate)
  (define type 'grammar)
  (define (generate)
    (generate-term rbtrees t 5)))

(module+ enum-mod
  (provide generate get-generator type)
  (define (get-generator) generate)
  (define type 'enum)
  (define (generate [p-value 0.25])
    (generate-term rbtrees t #:i-th (pick-an-index p-value))))

(module+ ordered-mod
  (provide generate get-generator type)
  (define (get-generator)
    (let ([index 0])
      (λ () (begin0
              (generate index)
              (set! index (add1 index))))))
  (define type 'ordered)
  (define (generate [index 0])
    (generate-term rbtrees t #:i-th index)))

(module+ check-mod
  (provide check)
  (define (check t)
    (or (not t)
        (implies (judgment-holds (rb-tree ,t)) 
                 (ins-preserves-rb-tree t)))))
