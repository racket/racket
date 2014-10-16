#lang typed/racket/base
(provide partitions set-partitions-cache)
(require math/private/number-theory/types
         (only-in racket/vector vector-copy))

;; The number of partitions of 4 is 5, since the number 4 can 
;; be written as a sum of natural numbers in 5 ways:
;;    4 = 3+1 = 2+2 = 2+1+1 = 1+1+1

;; http://en.wikipedia.org/wiki/Partition_(number_theory)

;;; Partitions are computed using Euler's algorithm:

;;                k            k(3k+1)              k(3k-1)
;; p(n) = sum (-1)   [ p( n - --------- ) + p( n - --------- ) ]
;;        k>=1                    2                    2

;; The formula is a recurence relation, so we use a cache to
;; avoid unnecessary computations.

; partitions store previously computed values in cache.
(: cache-size : Nonnegative-Integer)
(define cache-size 128)
(: cache (Vectorof Integer))
(define cache (make-vector cache-size 0))
(vector-set! cache 0 1)  ; p(0) = 1


(: set-partitions-cache : Index -> Void)
; Set the cache of partitions to size n.
; There are room for the values p(0),...,p(n-1).
(define (set-partitions-cache n)
  (cond [(< n 1)           (void)]
        [(< n cache-size)  (shrink-cache n)]
        [(= n cache-size)  (void)]
        [else              (grow-cache n)]))

(: shrink-cache : Natural -> Void)
; if cache-size is larger than n, shrink the cache
(define (shrink-cache n)
  (cond [(< cache-size n) (void)]
        [else (define new-cache (vector-copy cache 0 n))
              (set! cache-size n)
              (set! cache new-cache)]))

(: grow-cache : Natural -> Void)
; if cache-size is smaller than n, grow the cache to size n
(define (grow-cache n)
  (cond [(> cache-size n) (void)]
        [else (define new-cache (make-vector n 0))
              (vector-copy! new-cache 0 cache)
              (set! cache-size n)
              (set! cache new-cache)]))

(: double-cache : Natural -> Void)
; if cache-size is smaller than n, double the cache size (repeatedly if needed)
(define (double-cache n)
  (: new-cache-size : Nonnegative-Integer -> Nonnegative-Integer)
  (define (new-cache-size size)
    (cond [(<= n size) size]
          [else       (new-cache-size (* 2 size))]))
  (when (< cache-size n) 
    (grow-cache (new-cache-size cache-size))))


(: partitions : Integer -> Integer)
; double cache, if necessary, then call p
(define (partitions n)
  (cond [(< n 0) 0]
        [else (double-cache (+ n 1))
              (p n)]))

(: p : Natural -> Integer)
; compute the number of partitions of n using Euler's algorithm
(define (p n)
  (define cached (vector-ref cache n))
  (cond [(zero? cached)
         (define pn (+ (loop1  1 (- n 1) 0)
                       (loop2 -1 (- n 2) 0)))
         (vector-set! cache n pn)
         pn]
        [else cached]))

(: loop1 : Integer Integer Integer -> Integer)
(define (loop1 k m s)
  (cond [(< m 0) s]
        [else (loop1 (+ k 1) 
                     (- m (+ (* 3 k) 1)) 
                     (if (odd? k) (+ s (p m)) (- s (p m))))]))

(: loop2 : Integer Integer Integer -> Integer)
(define (loop2 k m s)
  (cond [(< m 0) s]
        [else   (loop2 (- k 1)
                       (+ m (* 3 k) -2) 
                       (if (odd? k) (+ s (p m)) (- s (p m))))]))
