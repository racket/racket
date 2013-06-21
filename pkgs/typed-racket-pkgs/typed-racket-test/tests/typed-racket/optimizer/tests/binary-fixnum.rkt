#;#;
#<<END
TR opt: binary-fixnum.rkt 13:15 (vector-length v) -- vector-length
TR opt: binary-fixnum.rkt 13:2 (bitwise-and (vector-length v) 1) -- binary fixnum
END
""

#lang typed/scheme
#:optimize

(: f (All (X) ((Vectorof X) -> Natural)))
(define (f v)
  (bitwise-and (vector-length v) 1))
