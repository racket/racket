#;#;
#<<END
TR opt: define-call-float.rkt 2:16 (+ 1.0 2.0) -- binary float
END
""
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(define x (cons (+ 1.0 2.0) 3.0))
