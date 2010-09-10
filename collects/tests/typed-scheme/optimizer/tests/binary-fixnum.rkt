#;
(
binary-fixnum.rkt line 12 col 16 - vector-length - vector-length
binary-fixnum.rkt line 12 col 3 - bitwise-and - binary fixnum
)

#lang typed/scheme
#:optimize

(: f (All (X) ((Vectorof X) -> Natural)))
(define (f v)
  (bitwise-and (vector-length v) 1))
