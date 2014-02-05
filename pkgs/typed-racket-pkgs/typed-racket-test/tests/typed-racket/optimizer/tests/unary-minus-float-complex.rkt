#;#;
#<<END
TR opt: unary-minus-float-complex.rkt 1:0 (imag-part (- 0.0+0.0i)) -- complex accessor elimination
TR opt: unary-minus-float-complex.rkt 1:11 (- 0.0+0.0i) -- unboxed unary float complex
TR opt: unary-minus-float-complex.rkt 1:14 0.0+0.0i -- unboxed literal
TR opt: unary-minus-float-complex.rkt 1:14 0.0+0.0i -- unboxed literal
END
#<<END
-0.0

END
#lang typed/racket
#reader tests/typed-racket/optimizer/reset-port
(imag-part (- 0.0+0.0i))
