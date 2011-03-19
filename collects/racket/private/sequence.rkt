#lang racket/base

;; Simple sequence functions that are also good enough
;; for streams

(require "for.rkt")

(provide sequence-andmap
         sequence-ormap
         sequence-for-each
         sequence-fold
         sequence-count)

(define (sequence-andmap f s)
  (unless (procedure? f) (raise-type-error 'sequence-andmap "procedure" f))
  (unless (sequence? s) (raise-type-error 'sequence-andmap "sequence" s))
  (for/and ([vs (in-values*-sequence s)])
    (if (list? vs)
        (apply f vs)
        (f vs))))

(define (sequence-ormap f s)
  (unless (procedure? f) (raise-type-error 'sequence-ormap "procedure" f))
  (unless (sequence? s) (raise-type-error 'sequence-ormap "sequence" s))
  (for/or ([vs (in-values*-sequence s)])
    (if (list? vs)
        (apply f vs)
        (f vs))))

(define (sequence-for-each f s)
  (unless (procedure? f) (raise-type-error 'sequence-for-each "procedure" f))
  (unless (sequence? s) (raise-type-error 'sequence-for-each "sequence" s))
  (for ([vs (in-values*-sequence s)])
    (if (list? vs)
        (apply f vs)
        (f vs))))

(define (sequence-fold f i s)
  (unless (procedure? f) (raise-type-error 'sequence-fold "procedure" f))
  (unless (sequence? s) (raise-type-error 'sequence-fold "sequence" s))
  (for/fold ([i i]) ([vs (in-values*-sequence s)])
    (if (list? vs)
        (apply f i vs)
        (f i vs))))

(define (sequence-count f s)
  (unless (procedure? f) (raise-type-error 'sequence-count "procedure" f))
  (unless (sequence? s) (raise-type-error 'sequence-count "sequence" s))
  (for/fold ([i 0]) ([vs (in-values*-sequence s)])
    (if (if (list? vs)
            (apply f vs)
            (f vs))
        (add1 i)
        i)))
