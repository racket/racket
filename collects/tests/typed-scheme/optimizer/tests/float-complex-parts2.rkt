#;
(
TR opt: float-complex-parts2.rkt 34:0 (real-part (+ 1.0+2.0i 2.0+4.0i)) -- unboxed float complex
TR opt: float-complex-parts2.rkt 34:11 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-parts2.rkt 34:11 (+ 1.0+2.0i 2.0+4.0i) -- unboxed float complex
TR opt: float-complex-parts2.rkt 34:14 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 34:23 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 35:0 (unsafe-flreal-part (+ 1.0+2.0i 2.0+4.0i)) -- unboxed float complex
TR opt: float-complex-parts2.rkt 35:20 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-parts2.rkt 35:20 (+ 1.0+2.0i 2.0+4.0i) -- unboxed float complex
TR opt: float-complex-parts2.rkt 35:23 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 35:32 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 36:0 (imag-part (+ 1.0+2.0i 2.0+4.0i)) -- unboxed float complex
TR opt: float-complex-parts2.rkt 36:11 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-parts2.rkt 36:11 (+ 1.0+2.0i 2.0+4.0i) -- unboxed float complex
TR opt: float-complex-parts2.rkt 36:14 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 36:23 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 37:0 (unsafe-flimag-part (+ 1.0+2.0i 2.0+4.0i)) -- unboxed float complex
TR opt: float-complex-parts2.rkt 37:20 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-parts2.rkt 37:20 (+ 1.0+2.0i 2.0+4.0i) -- unboxed float complex
TR opt: float-complex-parts2.rkt 37:23 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 37:32 2.0+4.0i -- unboxed literal
3.0
3.0
6.0
6.0
)

#lang typed/scheme
#:optimize

(require racket/unsafe/ops)

(real-part (+ 1.0+2.0i 2.0+4.0i))
(unsafe-flreal-part (+ 1.0+2.0i 2.0+4.0i))
(imag-part (+ 1.0+2.0i 2.0+4.0i))
(unsafe-flimag-part (+ 1.0+2.0i 2.0+4.0i))
