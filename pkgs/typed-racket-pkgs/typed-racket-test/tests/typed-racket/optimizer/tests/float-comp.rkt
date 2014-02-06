#;#;
#<<END
TR opt: float-comp.rkt 2:0 (< 1.0 2.0) -- binary float comp
END
#<<END
#t

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(< 1.0 2.0)
