#;
(
TR opt: binary-fixnum.rkt 12:2 (bitwise-and (vector-length v) 1) -- binary fixnum
TR opt: binary-fixnum.rkt 12:15 (vector-length v) -- vector-length
)

#lang typed/scheme
#:optimize

(: f (All (X) ((Vectorof X) -> Natural)))
(define (f v)
  (bitwise-and (vector-length v) 1))
