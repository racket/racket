#lang racket/base

(require redex/examples/let-poly
         (only-in redex/private/generate-term pick-an-index)
         redex/reduction-semantics
         racket/bool)

(provide (all-defined-out))

(module+ adhoc-mod
  (provide generate get-generator type)
  (define (get-generator) generate)
  (define type 'grammar)
  (define (generate)
    (generate-term stlc M 5)))

(module+ enum-mod
  (provide generate get-generator type)
  (define (get-generator) generate)
  (define type 'enum)
  (define (generate [p-value 0.035])
    (generate-term stlc M #:i-th (pick-an-index p-value))))

(module+ ordered-mod
  (provide generate get-generator type)
  (define (get-generator)
    (let ([index 0])
      (λ () (begin0
              (generate index)
              (set! index (add1 index))))))
  (define type 'ordered)
  (define (generate [index 0])
    (generate-term stlc M #:i-th index)))

(module+ check-mod
  (require (only-in redex/examples/stlc-tests-lib consistent-with?))
  (provide check)
  
  #|
   Check to see if a combination of preservation
   and progress holds for the first 100 terms
   reachable from the given term.
  |#

  (define (check M)
  (or (not M)
      (let ([t-type (type-check M)])
        (implies
         t-type
         (let loop ([Σ+M `(· ,M)] [n 100])
           (define new-type (type-check (list-ref Σ+M 1) (list-ref Σ+M 0)))
           (and (consistent-with? t-type new-type)
                (or (v? (list-ref Σ+M 1))
                    (let ([red-res (apply-reduction-relation red Σ+M)])
                      (and (= (length red-res) 1)
                           (let ([red-t (car red-res)])
                             (or (equal? red-t "error")
                                 (zero? n) (loop red-t (- n 1))))))))))))))

