#;
(
zero.rkt line 13 col 1 - zero? - fixnum zero?
zero.rkt line 14 col 8 - sqrt - unary float
zero.rkt line 14 col 1 - zero? - float zero?
#f
#f
)

#lang typed/scheme
#:optimize
(require racket/unsafe/ops)
(zero? 1)
(zero? (sqrt 3.0))
