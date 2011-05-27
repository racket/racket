#;
(
float-complex-parts2.rkt 34:1 real-part -- unboxed float complex
float-complex-parts2.rkt 34:11 (#%app + (quote 1.0+2.0i) (quote 2.0+4.0i)) -- unboxed float complex
float-complex-parts2.rkt 34:12 + -- unboxed binary float complex
float-complex-parts2.rkt 34:14 1.0+2.0i -- unboxed literal
float-complex-parts2.rkt 34:23 2.0+4.0i -- unboxed literal
float-complex-parts2.rkt 35:1 unsafe-flreal-part -- unboxed float complex
float-complex-parts2.rkt 35:20 (#%app + (quote 1.0+2.0i) (quote 2.0+4.0i)) -- unboxed float complex
float-complex-parts2.rkt 35:21 + -- unboxed binary float complex
float-complex-parts2.rkt 35:23 1.0+2.0i -- unboxed literal
float-complex-parts2.rkt 35:32 2.0+4.0i -- unboxed literal
float-complex-parts2.rkt 36:1 imag-part -- unboxed float complex
float-complex-parts2.rkt 36:11 (#%app + (quote 1.0+2.0i) (quote 2.0+4.0i)) -- unboxed float complex
float-complex-parts2.rkt 36:12 + -- unboxed binary float complex
float-complex-parts2.rkt 36:14 1.0+2.0i -- unboxed literal
float-complex-parts2.rkt 36:23 2.0+4.0i -- unboxed literal
float-complex-parts2.rkt 37:1 unsafe-flimag-part -- unboxed float complex
float-complex-parts2.rkt 37:20 (#%app + (quote 1.0+2.0i) (quote 2.0+4.0i)) -- unboxed float complex
float-complex-parts2.rkt 37:21 + -- unboxed binary float complex
float-complex-parts2.rkt 37:23 1.0+2.0i -- unboxed literal
float-complex-parts2.rkt 37:32 2.0+4.0i -- unboxed literal
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
