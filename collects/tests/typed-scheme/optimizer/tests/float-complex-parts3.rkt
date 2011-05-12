#;
(
float-complex-parts3.rkt 42:3 1.0+2.0i -- unboxed literal
float-complex-parts3.rkt 42:26 2.0+4.0i -- unboxed literal
float-complex-parts3.rkt 42:35 3.0+6.0i -- unboxed literal
float-complex-parts3.rkt 42:24 + -- unboxed binary float complex
float-complex-parts3.rkt 42:13 real-part -- unboxed unary float complex
float-complex-parts3.rkt 42:1 + -- unboxed binary float complex
float-complex-parts3.rkt 42:0 (#%app + (quote 1.0+2.0i) (#%app real-part (#%app + (quote 2.0+4.0i) (quote 3.0+6.0i)))) -- unboxed float complex
float-complex-parts3.rkt 43:3 1.0+2.0i -- unboxed literal
float-complex-parts3.rkt 43:35 2.0+4.0i -- unboxed literal
float-complex-parts3.rkt 43:44 3.0+6.0i -- unboxed literal
float-complex-parts3.rkt 43:33 + -- unboxed binary float complex
float-complex-parts3.rkt 43:13 unsafe-flreal-part -- unboxed unary float complex
float-complex-parts3.rkt 43:1 + -- unboxed binary float complex
float-complex-parts3.rkt 43:0 (#%app + (quote 1.0+2.0i) (#%app unsafe-flreal-part (#%app + (quote 2.0+4.0i) (quote 3.0+6.0i)))) -- unboxed float complex
float-complex-parts3.rkt 44:3 1.0+2.0i -- unboxed literal
float-complex-parts3.rkt 44:26 2.0+4.0i -- unboxed literal
float-complex-parts3.rkt 44:35 3.0+6.0i -- unboxed literal
float-complex-parts3.rkt 44:24 + -- unboxed binary float complex
float-complex-parts3.rkt 44:13 imag-part -- unboxed unary float complex
float-complex-parts3.rkt 44:1 + -- unboxed binary float complex
float-complex-parts3.rkt 44:0 (#%app + (quote 1.0+2.0i) (#%app imag-part (#%app + (quote 2.0+4.0i) (quote 3.0+6.0i)))) -- unboxed float complex
float-complex-parts3.rkt 45:3 1.0+2.0i -- unboxed literal
float-complex-parts3.rkt 45:35 2.0+4.0i -- unboxed literal
float-complex-parts3.rkt 45:44 3.0+6.0i -- unboxed literal
float-complex-parts3.rkt 45:33 + -- unboxed binary float complex
float-complex-parts3.rkt 45:13 unsafe-flimag-part -- unboxed unary float complex
float-complex-parts3.rkt 45:1 + -- unboxed binary float complex
float-complex-parts3.rkt 45:0 (#%app + (quote 1.0+2.0i) (#%app unsafe-flimag-part (#%app + (quote 2.0+4.0i) (quote 3.0+6.0i)))) -- unboxed float complex
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
