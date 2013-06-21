#;#;
#<<END
TR opt: float-complex-parts3.rkt 41:0 (+ 1.0+2.0i (real-part (+ 2.0+4.0i 3.0+6.0i))) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 41:12 (real-part (+ 2.0+4.0i 3.0+6.0i)) -- unboxed unary float complex
TR opt: float-complex-parts3.rkt 41:23 (+ 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 41:26 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 41:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 41:35 3.0+6.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 42:0 (+ 1.0+2.0i (unsafe-flreal-part (+ 2.0+4.0i 3.0+6.0i))) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 42:12 (unsafe-flreal-part (+ 2.0+4.0i 3.0+6.0i)) -- unboxed unary float complex
TR opt: float-complex-parts3.rkt 42:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 42:32 (+ 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 42:35 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 42:44 3.0+6.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 43:0 (+ 1.0+2.0i (imag-part (+ 2.0+4.0i 3.0+6.0i))) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 43:12 (imag-part (+ 2.0+4.0i 3.0+6.0i)) -- unboxed unary float complex
TR opt: float-complex-parts3.rkt 43:23 (+ 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 43:26 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 43:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 43:35 3.0+6.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 44:0 (+ 1.0+2.0i (unsafe-flimag-part (+ 2.0+4.0i 3.0+6.0i))) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 44:12 (unsafe-flimag-part (+ 2.0+4.0i 3.0+6.0i)) -- unboxed unary float complex
TR opt: float-complex-parts3.rkt 44:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 44:32 (+ 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 44:35 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 44:44 3.0+6.0i -- unboxed literal
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
