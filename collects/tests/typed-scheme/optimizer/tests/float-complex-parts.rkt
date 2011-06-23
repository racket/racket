#;
(
TR opt: float-complex-parts.rkt 17:0 (real-part 1.0+2.0i) -- unboxed float complex
TR opt: float-complex-parts.rkt 17:11 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts.rkt 18:0 (imag-part 1.0+2.0i) -- unboxed float complex
TR opt: float-complex-parts.rkt 18:11 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts.rkt 19:0 (real-part 1.0+2.0i) -- unboxed float complex
TR opt: float-complex-parts.rkt 19:11 1.0+2.0i -- unboxed literal
1.0
2.0
1.0
)

#lang typed/scheme
#:optimize

(real-part 1.0+2.0i)
(imag-part 1+2.0i)
(real-part 1.0+2i)
