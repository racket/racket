#;
(
TR opt: float-complex-parts3.rkt 38:0 (+ 1.0+2.0i (real-part (+ 2.0+4.0i 3.0+6.0i))) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 38:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 38:12 (real-part (+ 2.0+4.0i 3.0+6.0i)) -- unboxed unary float complex
TR opt: float-complex-parts3.rkt 38:23 (+ 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 38:26 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 38:35 3.0+6.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 39:0 (+ 1.0+2.0i (unsafe-flreal-part (+ 2.0+4.0i 3.0+6.0i))) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 39:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 39:12 (unsafe-flreal-part (+ 2.0+4.0i 3.0+6.0i)) -- unboxed unary float complex
TR opt: float-complex-parts3.rkt 39:32 (+ 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 39:35 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 39:44 3.0+6.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 40:0 (+ 1.0+2.0i (imag-part (+ 2.0+4.0i 3.0+6.0i))) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 40:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 40:12 (imag-part (+ 2.0+4.0i 3.0+6.0i)) -- unboxed unary float complex
TR opt: float-complex-parts3.rkt 40:23 (+ 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 40:26 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 40:35 3.0+6.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 41:0 (+ 1.0+2.0i (unsafe-flimag-part (+ 2.0+4.0i 3.0+6.0i))) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 41:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 41:12 (unsafe-flimag-part (+ 2.0+4.0i 3.0+6.0i)) -- unboxed unary float complex
TR opt: float-complex-parts3.rkt 41:32 (+ 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 41:35 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 41:44 3.0+6.0i -- unboxed literal
6.0+2.0i
6.0+2.0i
11.0+2.0i
11.0+2.0i
)

#lang typed/scheme
#:optimize

(require racket/unsafe/ops)

(+ 1.0+2.0i (real-part (+ 2.0+4.0i 3.0+6.0i)))
(+ 1.0+2.0i (unsafe-flreal-part (+ 2.0+4.0i 3.0+6.0i)))
(+ 1.0+2.0i (imag-part (+ 2.0+4.0i 3.0+6.0i)))
(+ 1.0+2.0i (unsafe-flimag-part (+ 2.0+4.0i 3.0+6.0i)))
