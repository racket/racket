#;
(
TR opt: float-complex-parts3.rkt 42:0 (+ 1.0+2.0i (real-part (+ 2.0+4.0i 3.0+6.0i))) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 42:0 (+ 1.0+2.0i (real-part (+ 2.0+4.0i 3.0+6.0i))) -- unboxed float complex
TR opt: float-complex-parts3.rkt 42:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 42:12 (real-part (+ 2.0+4.0i 3.0+6.0i)) -- unboxed unary float complex
TR opt: float-complex-parts3.rkt 42:23 (+ 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 42:26 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 42:35 3.0+6.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 43:0 (+ 1.0+2.0i (unsafe-flreal-part (+ 2.0+4.0i 3.0+6.0i))) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 43:0 (+ 1.0+2.0i (unsafe-flreal-part (+ 2.0+4.0i 3.0+6.0i))) -- unboxed float complex
TR opt: float-complex-parts3.rkt 43:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 43:12 (unsafe-flreal-part (+ 2.0+4.0i 3.0+6.0i)) -- unboxed unary float complex
TR opt: float-complex-parts3.rkt 43:32 (+ 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 43:35 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 43:44 3.0+6.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 44:0 (+ 1.0+2.0i (imag-part (+ 2.0+4.0i 3.0+6.0i))) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 44:0 (+ 1.0+2.0i (imag-part (+ 2.0+4.0i 3.0+6.0i))) -- unboxed float complex
TR opt: float-complex-parts3.rkt 44:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 44:12 (imag-part (+ 2.0+4.0i 3.0+6.0i)) -- unboxed unary float complex
TR opt: float-complex-parts3.rkt 44:23 (+ 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 44:26 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 44:35 3.0+6.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 45:0 (+ 1.0+2.0i (unsafe-flimag-part (+ 2.0+4.0i 3.0+6.0i))) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 45:0 (+ 1.0+2.0i (unsafe-flimag-part (+ 2.0+4.0i 3.0+6.0i))) -- unboxed float complex
TR opt: float-complex-parts3.rkt 45:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 45:12 (unsafe-flimag-part (+ 2.0+4.0i 3.0+6.0i)) -- unboxed unary float complex
TR opt: float-complex-parts3.rkt 45:32 (+ 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 45:35 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 45:44 3.0+6.0i -- unboxed literal
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
