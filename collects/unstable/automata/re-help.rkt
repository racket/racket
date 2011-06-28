#lang racket/base
(struct re-transformer (>re))
(provide (struct-out re-transformer))

(require (for-syntax racket/base))
(define-syntax (nullset stx) (raise-syntax-error 'nullset "Outside re" stx))
(define-syntax (epsilon stx) (raise-syntax-error 'epsilon "Outside re" stx))
(define-syntax (complement stx) (raise-syntax-error 'complement "Outside re" stx))
(define-syntax (seq stx) (raise-syntax-error 'seq "Outside re" stx))
(define-syntax (union stx) (raise-syntax-error 'union "Outside re" stx))
(define-syntax (star stx) (raise-syntax-error 'star "Outside re" stx))
(define-syntax (dseq stx) (raise-syntax-error 'dseq "Outside re" stx))
(define-syntax (rec stx) (raise-syntax-error 'rec "Outside re" stx))

(provide nullset epsilon complement seq union star dseq rec)
