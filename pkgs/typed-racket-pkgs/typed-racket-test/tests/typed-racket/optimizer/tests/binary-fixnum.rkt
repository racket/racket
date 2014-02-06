#;#;
#<<END
TR opt: binary-fixnum.rkt 4:15 (vector-length v) -- vector-length
TR opt: binary-fixnum.rkt 4:2 (bitwise-and (vector-length v) 1) -- binary fixnum
END
""
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(: f (All (X) ((Vectorof X) -> Natural)))
(define (f v)
  (bitwise-and (vector-length v) 1))
