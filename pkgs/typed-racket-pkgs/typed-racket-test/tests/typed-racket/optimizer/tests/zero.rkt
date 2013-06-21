#;#;
#<<END
TR opt: zero.rkt 16:0 (zero? 1) -- fixnum zero?
TR opt: zero.rkt 17:7 (sqrt 3.0) -- unary float
TR opt: zero.rkt 17:0 (zero? (sqrt 3.0)) -- float zero?
END
#<<END
#f
#f

END

#lang typed/scheme
#:optimize

(zero? 1)
(zero? (sqrt 3.0))
