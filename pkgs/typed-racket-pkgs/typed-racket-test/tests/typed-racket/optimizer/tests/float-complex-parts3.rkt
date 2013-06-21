#;#;
#<<END
TR opt: float-complex-parts3.rkt 44:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 44:26 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 44:35 3.0+6.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 44:23 (+ 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 44:12 (real-part (+ 2.0+4.0i 3.0+6.0i)) -- unboxed unary float complex
TR opt: float-complex-parts3.rkt 44:0 (+ 1.0+2.0i (real-part (+ 2.0+4.0i 3.0+6.0i))) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 45:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 45:35 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 45:44 3.0+6.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 45:32 (+ 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 45:12 (unsafe-flreal-part (+ 2.0+4.0i 3.0+6.0i)) -- unboxed unary float complex
TR opt: float-complex-parts3.rkt 45:0 (+ 1.0+2.0i (unsafe-flreal-part (+ 2.0+4.0i 3.0+6.0i))) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 46:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 46:26 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 46:35 3.0+6.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 46:23 (+ 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 46:12 (imag-part (+ 2.0+4.0i 3.0+6.0i)) -- unboxed unary float complex
TR opt: float-complex-parts3.rkt 46:0 (+ 1.0+2.0i (imag-part (+ 2.0+4.0i 3.0+6.0i))) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 47:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 47:35 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 47:44 3.0+6.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 47:32 (+ 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 47:12 (unsafe-flimag-part (+ 2.0+4.0i 3.0+6.0i)) -- unboxed unary float complex
TR opt: float-complex-parts3.rkt 47:0 (+ 1.0+2.0i (unsafe-flimag-part (+ 2.0+4.0i 3.0+6.0i))) -- unboxed binary float complex

END
#<<END
6.0+2.0i
6.0+2.0i
11.0+2.0i
11.0+2.0i

END

#lang typed/scheme
#:optimize

(require racket/unsafe/ops)

(+ 1.0+2.0i (real-part (+ 2.0+4.0i 3.0+6.0i)))
(+ 1.0+2.0i (unsafe-flreal-part (+ 2.0+4.0i 3.0+6.0i)))
(+ 1.0+2.0i (imag-part (+ 2.0+4.0i 3.0+6.0i)))
(+ 1.0+2.0i (unsafe-flimag-part (+ 2.0+4.0i 3.0+6.0i)))
