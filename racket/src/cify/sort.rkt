#lang racket/base

(provide in-sorted-hash
         in-sorted-hash-keys
         in-sorted-hash-values

         compare)

(define (in-sorted-hash ht <?)
  (define l (sort (hash-keys ht) <?))
  (in-parallel l
               (map (lambda (k) (hash-ref ht k)) l)))

(define (in-sorted-hash-keys ht <?)
  (sort (hash-keys ht) <?))

(define (in-sorted-hash-values ht <?)
  (define l (hash-keys ht))
  (sort (map (lambda (k) (hash-ref ht k)) l) <?))

(define (compare <? sel)
  (lambda (a b)
    (<? (sel a) (sel b))))
