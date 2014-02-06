#;#;
#<<END
TR opt: nested-float.rkt 2:0 (+ 2.0 (+ 3.0 4.0)) -- binary float
TR opt: nested-float.rkt 2:7 (+ 3.0 4.0) -- binary float
END
#<<END
9.0

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(+ 2.0 (+ 3.0 4.0))
