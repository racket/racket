#;#;
#<<END
TR opt: float-complex-parts.rkt 20:0 (real-part 1.0+2.0i) -- complex accessor elimination
TR opt: float-complex-parts.rkt 20:11 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts.rkt 21:0 (imag-part 1.0+2.0i) -- complex accessor elimination
TR opt: float-complex-parts.rkt 21:11 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts.rkt 22:0 (real-part 1.0+2.0i) -- complex accessor elimination
TR opt: float-complex-parts.rkt 22:11 1.0+2.0i -- unboxed literal
END
#<<END
1.0
2.0
1.0

END

#lang typed/scheme
#:optimize

(real-part 1.0+2.0i)
(imag-part 1+2.0i)
(real-part 1.0+2i)
