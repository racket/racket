#lang racket/base
(require "../utils/utils.rkt" racket/unsafe/ops)
(require (rep type-rep) (contract-req))

(provide (except-out (all-defined-out) current-seen-mark))

(define current-seen-mark (make-continuation-mark-key 'current-seen))
(define (current-seen)
  (continuation-mark-set-first #f current-seen-mark null))
(define (currently-subtyping?)
  (continuation-mark-set-first #f current-seen-mark))
(define-syntax-rule (update-current-seen new-value body)
  (with-continuation-mark current-seen-mark new-value body))

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

