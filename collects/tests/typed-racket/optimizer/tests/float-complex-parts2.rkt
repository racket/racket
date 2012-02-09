#;
(
TR opt: float-complex-parts2.rkt 42:14 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 42:23 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 42:11 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-parts2.rkt 42:14 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 42:23 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 42:11 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-parts2.rkt 42:0 (real-part (+ 1.0+2.0i 2.0+4.0i)) -- complex accessor elimination
TR opt: float-complex-parts2.rkt 43:23 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 43:32 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 43:20 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-parts2.rkt 43:23 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 43:32 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 43:20 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-parts2.rkt 43:0 (unsafe-flreal-part (+ 1.0+2.0i 2.0+4.0i)) -- complex accessor elimination
TR opt: float-complex-parts2.rkt 44:14 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 44:23 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 44:11 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-parts2.rkt 44:14 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 44:23 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 44:11 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-parts2.rkt 44:0 (imag-part (+ 1.0+2.0i 2.0+4.0i)) -- complex accessor elimination
TR opt: float-complex-parts2.rkt 45:23 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 45:32 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 45:20 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-parts2.rkt 45:23 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 45:32 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts2.rkt 45:20 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-parts2.rkt 45:0 (unsafe-flimag-part (+ 1.0+2.0i 2.0+4.0i)) -- complex accessor elimination
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
