#lang racket/base
(require "../utils/utils.rkt")
(require (rep type-rep))

(provide (all-defined-out))

(define current-seen (make-parameter null))
(define (currently-subtyping?) (not (null? (current-seen))))

(define (seen-before s t) (cons (Type-seq s) (Type-seq t)))
(define (remember s t A) (cons (seen-before s t) A))
(define (seen? s t) (member (seen-before s t) (current-seen)))

