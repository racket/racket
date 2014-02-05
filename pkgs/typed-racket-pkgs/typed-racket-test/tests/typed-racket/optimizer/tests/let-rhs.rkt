#;#;
#<<END
TR opt: let-rhs.rkt 4:9 (+ 1.0 2.0) -- binary float
END
#<<END
3.0

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port



(let ((x (+ 1.0 2.0)))
  x)
