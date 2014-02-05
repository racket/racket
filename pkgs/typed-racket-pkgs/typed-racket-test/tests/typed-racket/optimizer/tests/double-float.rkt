#;#;
#<<END
TR opt: double-float.rkt 2:0 (+ 2.0 2.0 2.0) -- binary float
END
#<<END
6.0

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(+ 2.0 2.0 2.0)
