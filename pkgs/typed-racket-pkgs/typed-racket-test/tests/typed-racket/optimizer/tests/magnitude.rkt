#;#;
#<<END
TR opt: magnitude.rkt 4:0 (magnitude 3.0+4.0i) -- unboxed unary float complex
TR opt: magnitude.rkt 4:11 3.0+4.0i -- unboxed literal
END
#<<END
5.0

END
#lang typed/racket/base
#:optimize
#reader tests/typed-racket/optimizer/reset-port



(magnitude 3.0+4.0i)
