#;
(
float-complex-parts.rkt 17:1 real-part -- unboxed float complex
float-complex-parts.rkt 17:11 1.0+2.0i -- unboxed literal
float-complex-parts.rkt 18:1 imag-part -- unboxed float complex
float-complex-parts.rkt 18:11 1.0+2.0i -- unboxed literal
float-complex-parts.rkt 19:1 real-part -- unboxed float complex
float-complex-parts.rkt 19:11 1.0+2.0i -- unboxed literal
1.0
2.0
1.0
)

#lang typed/scheme
#:optimize

(real-part 1.0+2.0i)
(imag-part 1+2.0i)
(real-part 1.0+2i)
