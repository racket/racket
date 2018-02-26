#lang racket/base

;; For a hash table that's likely to be small, then a boxed immutable
;; hash table can be more efficient

(provide make-small-hasheq
         make-small-hasheqv
         small-hash-ref
         small-hash-set!
         small-hash-keys)

(define (make-small-hasheq)
  (box #hasheq()))

(define (make-small-hasheqv)
  (box #hasheqv()))

(define (small-hash-ref small-ht key default)
  (hash-ref (unbox small-ht) key default))

(define (small-hash-set! small-ht key val)
  (set-box! small-ht (hash-set (unbox small-ht) key val)))

(define (small-hash-keys small-ht)
  (hash-keys (unbox small-ht)))
