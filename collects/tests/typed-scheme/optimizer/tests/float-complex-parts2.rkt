#;
(
float-complex-parts2.rkt line 46 col 14 - 1.0+2.0i - unboxed literal
float-complex-parts2.rkt line 46 col 23 - 2.0+4.0i - unboxed literal
float-complex-parts2.rkt line 46 col 12 - + - unboxed binary float complex
float-complex-parts2.rkt line 46 col 11 - (#%app + (quote 1.0+2.0i) (quote 2.0+4.0i)) - unboxed float complex
float-complex-parts2.rkt line 46 col 14 - 1.0+2.0i - unboxed literal
float-complex-parts2.rkt line 46 col 23 - 2.0+4.0i - unboxed literal
float-complex-parts2.rkt line 46 col 12 - + - unboxed binary float complex
float-complex-parts2.rkt line 46 col 1 - real-part - unboxed float complex
float-complex-parts2.rkt line 47 col 23 - 1.0+2.0i - unboxed literal
float-complex-parts2.rkt line 47 col 32 - 2.0+4.0i - unboxed literal
float-complex-parts2.rkt line 47 col 21 - + - unboxed binary float complex
float-complex-parts2.rkt line 47 col 20 - (#%app + (quote 1.0+2.0i) (quote 2.0+4.0i)) - unboxed float complex
float-complex-parts2.rkt line 47 col 23 - 1.0+2.0i - unboxed literal
float-complex-parts2.rkt line 47 col 32 - 2.0+4.0i - unboxed literal
float-complex-parts2.rkt line 47 col 21 - + - unboxed binary float complex
float-complex-parts2.rkt line 47 col 1 - unsafe-flreal-part - unboxed float complex
float-complex-parts2.rkt line 48 col 14 - 1.0+2.0i - unboxed literal
float-complex-parts2.rkt line 48 col 23 - 2.0+4.0i - unboxed literal
float-complex-parts2.rkt line 48 col 12 - + - unboxed binary float complex
float-complex-parts2.rkt line 48 col 11 - (#%app + (quote 1.0+2.0i) (quote 2.0+4.0i)) - unboxed float complex
float-complex-parts2.rkt line 48 col 14 - 1.0+2.0i - unboxed literal
float-complex-parts2.rkt line 48 col 23 - 2.0+4.0i - unboxed literal
float-complex-parts2.rkt line 48 col 12 - + - unboxed binary float complex
float-complex-parts2.rkt line 48 col 1 - imag-part - unboxed float complex
float-complex-parts2.rkt line 49 col 23 - 1.0+2.0i - unboxed literal
float-complex-parts2.rkt line 49 col 32 - 2.0+4.0i - unboxed literal
float-complex-parts2.rkt line 49 col 21 - + - unboxed binary float complex
float-complex-parts2.rkt line 49 col 20 - (#%app + (quote 1.0+2.0i) (quote 2.0+4.0i)) - unboxed float complex
float-complex-parts2.rkt line 49 col 23 - 1.0+2.0i - unboxed literal
float-complex-parts2.rkt line 49 col 32 - 2.0+4.0i - unboxed literal
float-complex-parts2.rkt line 49 col 21 - + - unboxed binary float complex
float-complex-parts2.rkt line 49 col 1 - unsafe-flimag-part - unboxed float complex
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
