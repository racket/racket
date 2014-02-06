#;#;
#<<END
TR opt: define-float.rkt 2:10 (+ 1.0 2.0) -- binary float
END
""
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(define x (+ 1.0 2.0))
