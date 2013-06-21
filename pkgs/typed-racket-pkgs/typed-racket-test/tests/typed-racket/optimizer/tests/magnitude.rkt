#;#;
#<<END
TR opt: magnitude.rkt 16:0 (magnitude 3.0+4.0i) -- unboxed unary float complex
TR opt: magnitude.rkt 16:11 3.0+4.0i -- unboxed literal
END
#<<END
5.0

END

#lang typed/racket/base
#:optimize



(magnitude 3.0+4.0i)
