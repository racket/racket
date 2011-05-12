#;
(
float-complex-parts2.rkt 46:14 1.0+2.0i -- unboxed literal
float-complex-parts2.rkt 46:23 2.0+4.0i -- unboxed literal
float-complex-parts2.rkt 46:12 + -- unboxed binary float complex
float-complex-parts2.rkt 46:11 (#%app + (quote 1.0+2.0i) (quote 2.0+4.0i)) -- unboxed float complex
float-complex-parts2.rkt 46:14 1.0+2.0i -- unboxed literal
float-complex-parts2.rkt 46:23 2.0+4.0i -- unboxed literal
float-complex-parts2.rkt 46:12 + -- unboxed binary float complex
float-complex-parts2.rkt 46:1 real-part -- unboxed float complex
float-complex-parts2.rkt 47:23 1.0+2.0i -- unboxed literal
float-complex-parts2.rkt 47:32 2.0+4.0i -- unboxed literal
float-complex-parts2.rkt 47:21 + -- unboxed binary float complex
float-complex-parts2.rkt 47:20 (#%app + (quote 1.0+2.0i) (quote 2.0+4.0i)) -- unboxed float complex
float-complex-parts2.rkt 47:23 1.0+2.0i -- unboxed literal
float-complex-parts2.rkt 47:32 2.0+4.0i -- unboxed literal
float-complex-parts2.rkt 47:21 + -- unboxed binary float complex
float-complex-parts2.rkt 47:1 unsafe-flreal-part -- unboxed float complex
float-complex-parts2.rkt 48:14 1.0+2.0i -- unboxed literal
float-complex-parts2.rkt 48:23 2.0+4.0i -- unboxed literal
float-complex-parts2.rkt 48:12 + -- unboxed binary float complex
float-complex-parts2.rkt 48:11 (#%app + (quote 1.0+2.0i) (quote 2.0+4.0i)) -- unboxed float complex
float-complex-parts2.rkt 48:14 1.0+2.0i -- unboxed literal
float-complex-parts2.rkt 48:23 2.0+4.0i -- unboxed literal
float-complex-parts2.rkt 48:12 + -- unboxed binary float complex
float-complex-parts2.rkt 48:1 imag-part -- unboxed float complex
float-complex-parts2.rkt 49:23 1.0+2.0i -- unboxed literal
float-complex-parts2.rkt 49:32 2.0+4.0i -- unboxed literal
float-complex-parts2.rkt 49:21 + -- unboxed binary float complex
float-complex-parts2.rkt 49:20 (#%app + (quote 1.0+2.0i) (quote 2.0+4.0i)) -- unboxed float complex
float-complex-parts2.rkt 49:23 1.0+2.0i -- unboxed literal
float-complex-parts2.rkt 49:32 2.0+4.0i -- unboxed literal
float-complex-parts2.rkt 49:21 + -- unboxed binary float complex
float-complex-parts2.rkt 49:1 unsafe-flimag-part -- unboxed float complex
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
