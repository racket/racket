#;
(
float-complex-parts3.rkt line 42 col 3 - 1.0+2.0i - unboxed literal
float-complex-parts3.rkt line 42 col 26 - 2.0+4.0i - unboxed literal
float-complex-parts3.rkt line 42 col 35 - 3.0+6.0i - unboxed literal
float-complex-parts3.rkt line 42 col 24 - + - unboxed binary float complex
float-complex-parts3.rkt line 42 col 13 - real-part - unboxed unary float complex
float-complex-parts3.rkt line 42 col 1 - + - unboxed binary float complex
float-complex-parts3.rkt line 42 col 0 - (#%app + (quote 1.0+2.0i) (#%app real-part (#%app + (quote 2.0+4.0i) (quote 3.0+6.0i)))) - unboxed float complex
float-complex-parts3.rkt line 43 col 3 - 1.0+2.0i - unboxed literal
float-complex-parts3.rkt line 43 col 35 - 2.0+4.0i - unboxed literal
float-complex-parts3.rkt line 43 col 44 - 3.0+6.0i - unboxed literal
float-complex-parts3.rkt line 43 col 33 - + - unboxed binary float complex
float-complex-parts3.rkt line 43 col 13 - unsafe-flreal-part - unboxed unary float complex
float-complex-parts3.rkt line 43 col 1 - + - unboxed binary float complex
float-complex-parts3.rkt line 43 col 0 - (#%app + (quote 1.0+2.0i) (#%app unsafe-flreal-part (#%app + (quote 2.0+4.0i) (quote 3.0+6.0i)))) - unboxed float complex
float-complex-parts3.rkt line 44 col 3 - 1.0+2.0i - unboxed literal
float-complex-parts3.rkt line 44 col 26 - 2.0+4.0i - unboxed literal
float-complex-parts3.rkt line 44 col 35 - 3.0+6.0i - unboxed literal
float-complex-parts3.rkt line 44 col 24 - + - unboxed binary float complex
float-complex-parts3.rkt line 44 col 13 - imag-part - unboxed unary float complex
float-complex-parts3.rkt line 44 col 1 - + - unboxed binary float complex
float-complex-parts3.rkt line 44 col 0 - (#%app + (quote 1.0+2.0i) (#%app imag-part (#%app + (quote 2.0+4.0i) (quote 3.0+6.0i)))) - unboxed float complex
float-complex-parts3.rkt line 45 col 3 - 1.0+2.0i - unboxed literal
float-complex-parts3.rkt line 45 col 35 - 2.0+4.0i - unboxed literal
float-complex-parts3.rkt line 45 col 44 - 3.0+6.0i - unboxed literal
float-complex-parts3.rkt line 45 col 33 - + - unboxed binary float complex
float-complex-parts3.rkt line 45 col 13 - unsafe-flimag-part - unboxed unary float complex
float-complex-parts3.rkt line 45 col 1 - + - unboxed binary float complex
float-complex-parts3.rkt line 45 col 0 - (#%app + (quote 1.0+2.0i) (#%app unsafe-flimag-part (#%app + (quote 2.0+4.0i) (quote 3.0+6.0i)))) - unboxed float complex
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
