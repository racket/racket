#;#;
#<<END
TR opt: float-complex-parts3.rkt 4:0 (+ 1.0+2.0i (real-part (+ 2.0+4.0i 3.0+6.0i))) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 4:12 (real-part (+ 2.0+4.0i 3.0+6.0i)) -- complex accessor elimination
TR opt: float-complex-parts3.rkt 4:12 (real-part (+ 2.0+4.0i 3.0+6.0i)) -- float in complex ops
TR opt: float-complex-parts3.rkt 4:23 (+ 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 4:26 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 4:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 4:35 3.0+6.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 5:0 (+ 1.0+2.0i (unsafe-flreal-part (+ 2.0+4.0i 3.0+6.0i))) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 5:12 (unsafe-flreal-part (+ 2.0+4.0i 3.0+6.0i)) -- complex accessor elimination
TR opt: float-complex-parts3.rkt 5:12 (unsafe-flreal-part (+ 2.0+4.0i 3.0+6.0i)) -- float in complex ops
TR opt: float-complex-parts3.rkt 5:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 5:32 (+ 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 5:35 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 5:44 3.0+6.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 6:0 (+ 1.0+2.0i (imag-part (+ 2.0+4.0i 3.0+6.0i))) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 6:12 (imag-part (+ 2.0+4.0i 3.0+6.0i)) -- complex accessor elimination
TR opt: float-complex-parts3.rkt 6:12 (imag-part (+ 2.0+4.0i 3.0+6.0i)) -- float in complex ops
TR opt: float-complex-parts3.rkt 6:23 (+ 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 6:26 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 6:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 6:35 3.0+6.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 7:0 (+ 1.0+2.0i (unsafe-flimag-part (+ 2.0+4.0i 3.0+6.0i))) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 7:12 (unsafe-flimag-part (+ 2.0+4.0i 3.0+6.0i)) -- complex accessor elimination
TR opt: float-complex-parts3.rkt 7:12 (unsafe-flimag-part (+ 2.0+4.0i 3.0+6.0i)) -- float in complex ops
TR opt: float-complex-parts3.rkt 7:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 7:32 (+ 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 7:35 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 7:44 3.0+6.0i -- unboxed literal
END
#<<END
6.0+2.0i
6.0+2.0i
11.0+2.0i
11.0+2.0i

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(require racket/unsafe/ops)

(+ 1.0+2.0i (real-part (+ 2.0+4.0i 3.0+6.0i)))
(+ 1.0+2.0i (unsafe-flreal-part (+ 2.0+4.0i 3.0+6.0i)))
(+ 1.0+2.0i (imag-part (+ 2.0+4.0i 3.0+6.0i)))
(+ 1.0+2.0i (unsafe-flimag-part (+ 2.0+4.0i 3.0+6.0i)))
