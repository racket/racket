#;
(
TR opt: binary-fixnum.rkt 12:3 bitwise-and -- binary fixnum
TR opt: binary-fixnum.rkt 12:16 vector-length -- vector-length
)

#lang typed/scheme
#:optimize

(: f (All (X) ((Vectorof X) -> Natural)))
(define (f v)
  (bitwise-and (vector-length v) 1))
