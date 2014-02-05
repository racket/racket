#;#;
#<<END
TR opt: unary-float.rkt 2:0 (sin 2.0) -- unary float
END
#<<END
0.9092974268256817

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(sin 2.0)
