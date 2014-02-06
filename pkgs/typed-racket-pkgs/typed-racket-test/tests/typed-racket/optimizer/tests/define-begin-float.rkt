#;#;
#<<END
TR info: define-begin-float.rkt 1:18 display -- hidden parameter
TR opt: define-begin-float.rkt 1:26 (- 2.0 3.0) -- binary float
TR opt: define-begin-float.rkt 2:17 (* 2.0 3.0) -- binary float
END
"-1.0"
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port
(define a (begin (display (- 2.0 3.0))
                 (* 2.0 3.0)))
