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
  (unless (procedure? f) (raise-argument-error 'sequence-andmap "procedure?" f))
  (unless (sequence? s) (raise-argument-error 'sequence-andmap "sequence?" s))
  (for/and ([vs (in-values*-sequence s)])
    (if (list? vs)
        (apply f vs)
        (f vs))))

(define (sequence-ormap f s)
  (unless (procedure? f) (raise-argument-error 'sequence-ormap "procedure?" f))
  (unless (sequence? s) (raise-argument-error 'sequence-ormap "sequence?" s))
  (for/or ([vs (in-values*-sequence s)])
    (if (list? vs)
        (apply f vs)
        (f vs))))

(define (sequence-for-each f s)
  (unless (procedure? f) (raise-argument-error 'sequence-for-each "procedure?" f))
  (unless (sequence? s) (raise-argument-error 'sequence-for-each "sequence?" s))
  (for ([vs (in-values*-sequence s)])
    (if (list? vs)
        (apply f vs)
        (f vs))))

(define (sequence-fold f i s)
  (unless (procedure? f) (raise-argument-error 'sequence-fold "procedure?" f))
  (unless (sequence? s) (raise-argument-error 'sequence-fold "sequence?" s))
  (for/fold ([i i]) ([vs (in-values*-sequence s)])
    (if (list? vs)
        (apply f i vs)
        (f i vs))))

(define (sequence-count f s)
  (unless (procedure? f) (raise-argument-error 'sequence-count "procedure?" f))
  (unless (sequence? s) (raise-argument-error 'sequence-count "sequence?" s))
  (for/fold ([i 0]) ([vs (in-values*-sequence s)])
    (if (if (list? vs)
            (apply f vs)
            (f vs))
        (add1 i)
        i)))
