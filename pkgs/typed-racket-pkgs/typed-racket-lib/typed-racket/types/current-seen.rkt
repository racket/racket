#lang racket/base
(require "../utils/utils.rkt")
(require (rep type-rep) (contract-req))

(provide (except-out (all-defined-out) current-seen))
(provide/cond-contract [current-seen (parameter/c list?)])

(define current-seen (make-parameter null))
(define (currently-subtyping?) (not (null? (current-seen))))

(define (seen-before s t) (cons (Type-seq s) (Type-seq t)))
(define (remember s t A) (cons (seen-before s t) A))
(define (seen? s t) (member (seen-before s t) (current-seen)))

