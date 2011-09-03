#;
(
TR opt: float-complex-parts2.rkt 30:0 (real-part (+ 1.0+2.0i 2.0+4.0i)) -- complex accessor elimination
TR opt: float-complex-parts2.rkt 30:11 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-parts2.rkt 30:14 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 30:23 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 31:0 (unsafe-flreal-part (+ 1.0+2.0i 2.0+4.0i)) -- complex accessor elimination
TR opt: float-complex-parts2.rkt 31:20 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-parts2.rkt 31:23 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 31:32 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 32:0 (imag-part (+ 1.0+2.0i 2.0+4.0i)) -- complex accessor elimination
TR opt: float-complex-parts2.rkt 32:11 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-parts2.rkt 32:14 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 32:23 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 33:0 (unsafe-flimag-part (+ 1.0+2.0i 2.0+4.0i)) -- complex accessor elimination
TR opt: float-complex-parts2.rkt 33:20 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-parts2.rkt 33:23 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 33:32 2.0+4.0i -- unboxed literal
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
