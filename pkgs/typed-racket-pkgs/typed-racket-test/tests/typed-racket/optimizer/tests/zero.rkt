#;#;
#<<END
TR opt: zero.rkt 2:0 (zero? 1) -- fixnum zero?
TR opt: zero.rkt 3:0 (zero? (sqrt 3.0)) -- float zero?
TR opt: zero.rkt 3:7 (sqrt 3.0) -- unary float
END
#<<END
#f
#f

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(zero? 1)
(zero? (sqrt 3.0))
