#lang racket
(require tests/stress/stress)

; seqn-first
; This ignores the greater flexiblity of seqn-first to have more than single-valued sequences
(stress
 200
 ["seqn-first"
  (seqn-first (in-naturals))]
 ["for/or (val)"
  (define s (in-naturals))
  (for/or ([n s])
    n)]
 ["for/or"
  (for/or ([n (in-naturals)])
    n)])

; seqn-length
; The for/fold must be rewritten slightly differently for multi-valued
(stress
 20
 ["seqn-length"
  (seqn-length (in-range 2000))]
 ["for/fold (val)"
  (define s (in-range 2000))
  (for/fold ([len 0])
    ([i s])
    (add1 len))]
 ["for/fold"
  (for/fold ([len 0])
    ([i (in-range 2000)])
    (add1 len))])

; seqn-ref
; Ditto
(stress
 20
 ["seqn-ref"
  (seqn-ref (in-range 2000) 200)]
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

; seqn-andmap
; ditto
(stress
 20
 ["seqn-andmap"
  (seqn-andmap number? (in-range 2000))]
 ["for/and val"
  (define s (in-range 2000))
  (for/and ([e s])
    (number? e))]
 ["for/and"
  (for/and ([e (in-range 2000)])
    (number? e))])

; seqn-ormap
; ditto
(stress
 20
 ["seqn-ormap"
  (seqn-ormap string? (in-range 2000))]
 ["for/and val"
  (define s (in-range 2000))
  (for/or ([e s])
    (string? e))]
 ["for/and"
  (for/or ([e (in-range 2000)])
    (string? e))])

; seqn-fold
; The for/fold must be rewritten slightly differently for multi-valued
(stress
 20
 ["seqn-fold"
  (seqn-fold + 0 (in-range 2000))]
 ["for/fold (val)"
  (define s (in-range 2000))
  (for/fold ([sum 0])
    ([i s])
    (+ i sum))]
 ["for/fold"
  (for/fold ([sum 0])
    ([i (in-range 2000)])
    (+ i sum))])

; seqn-count
; The for/fold must be rewritten slightly differently for multi-valued
(stress
 20
 ["seqn-count"
  (seqn-count even? (in-range 2000))]
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

