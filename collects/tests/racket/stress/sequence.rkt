#lang racket
(require tests/stress)

;; stream-first
;; This ignores the greater flexiblity of stream-first to have more than
;; single-valued sequences
(stress
 200
 ["stream-first"
  (stream-first (in-naturals))]
 ["for/or (val)"
  (define s (in-naturals))
  (for/or ([n s])
    n)]
 ["for/or"
  (for/or ([n (in-naturals)])
    n)])

;; stream-length
;; The for/fold must be rewritten slightly differently for multi-valued
(stress
 20
 ["stream-length"
  (stream-length (in-range 2000))]
 ["for/fold (val)"
  (define s (in-range 2000))
  (for/fold ([len 0])
    ([i s])
    (add1 len))]
 ["for/fold"
  (for/fold ([len 0])
    ([i (in-range 2000)])
    (add1 len))])

;; stream-ref
;; Ditto
(stress
 20
 ["stream-ref"
  (stream-ref (in-range 2000) 200)]
 ["for/or val"
  (define s (in-range 2000))
  (for/or ([e s]
           [i (in-naturals)]
           #:when (i . = . 199))
    e)]
 ["for/or"
  (for/or ([e (in-range 2000)]
           [i (in-naturals)]
           #:when (i . = . 199))
    e)])

;; stream-andmap
;; ditto
(stress
 20
 ["stream-andmap"
  (stream-andmap number? (in-range 2000))]
 ["for/and val"
  (define s (in-range 2000))
  (for/and ([e s])
    (number? e))]
 ["for/and"
  (for/and ([e (in-range 2000)])
    (number? e))])

;; stream-ormap
;; ditto
(stress
 20
 ["stream-ormap"
  (stream-ormap string? (in-range 2000))]
 ["for/and val"
  (define s (in-range 2000))
  (for/or ([e s])
    (string? e))]
 ["for/and"
  (for/or ([e (in-range 2000)])
    (string? e))])

;; stream-fold
;; The for/fold must be rewritten slightly differently for multi-valued
(stress
 20
 ["stream-fold"
  (stream-fold + 0 (in-range 2000))]
 ["for/fold (val)"
  (define s (in-range 2000))
  (for/fold ([sum 0])
    ([i s])
    (+ i sum))]
 ["for/fold"
  (for/fold ([sum 0])
    ([i (in-range 2000)])
    (+ i sum))])

;; stream-count
;; The for/fold must be rewritten slightly differently for multi-valued
(stress
 20
 ["stream-count"
  (stream-count even? (in-range 2000))]
 ["for/fold (val)"
  (define s (in-range 2000))
  (for/fold ([num 0])
    ([i s]
     #:when (even? i))
    (add1 num))]
 ["for/fold"
  (for/fold ([num 0])
    ([i (in-range 2000)]
     #:when (even? i))
    (add1 num))])
