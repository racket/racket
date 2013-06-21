#;#;
#<<END
TR opt: magnitude.rkt 17:11 3.0+4.0i -- unboxed literal
TR opt: magnitude.rkt 17:0 (magnitude 3.0+4.0i) -- unboxed unary float complex

END
#<<END
5.0

END

#lang typed/racket/base
#:optimize



(magnitude 3.0+4.0i)
