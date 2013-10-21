#lang racket/base
(require "../utils/utils.rkt" racket/unsafe/ops)
(require (rep type-rep) (contract-req))

(provide (except-out (all-defined-out) current-seen))
(provide/cond-contract [current-seen (parameter/c list?)])

(define current-seen (make-parameter null))
(define (currently-subtyping?) (not (null? (current-seen))))
(define (seen-before s t) (cons (Type-seq s) (Type-seq t)))

(define (remember s t A) 
  (if (or (Mu? s) (Mu? t)
          (Name? s) (Name? t)
          (Struct? s) (Struct? t) 
          (App? s) (App? t))
      (cons (seen-before s t) A)
      A))
(define (seen? ss st cs)
  (for/or ([i (in-list cs)])
    (and (eq? ss (unsafe-car i)) (eq? st (unsafe-cdr i)))))

