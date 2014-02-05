#;#;
#<<END
TR opt: float-complex-parts.rkt 2:0 (real-part 1.0+2.0i) -- complex accessor elimination
TR opt: float-complex-parts.rkt 2:11 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts.rkt 3:0 (imag-part 1.0+2.0i) -- complex accessor elimination
TR opt: float-complex-parts.rkt 3:11 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts.rkt 4:0 (real-part 1.0+2.0i) -- complex accessor elimination
TR opt: float-complex-parts.rkt 4:11 1.0+2.0i -- unboxed literal
END
#<<END
1.0
2.0
1.0

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(real-part 1.0+2.0i)
(imag-part 1+2.0i)
(real-part 1.0+2i)
