#;
(
float-complex-conjugate-top.rkt 14:0 (#%app conjugate (#%app + (quote 1.0+2.0i) (quote 2.0+4.0i))) -- unboxed float complex
float-complex-conjugate-top.rkt 14:1 conjugate -- unboxed unary float complex
float-complex-conjugate-top.rkt 14:12 + -- unboxed binary float complex
float-complex-conjugate-top.rkt 14:14 1.0+2.0i -- unboxed literal
float-complex-conjugate-top.rkt 14:23 2.0+4.0i -- unboxed literal
3.0-6.0i
)

#lang typed/scheme
#:optimize

(conjugate (+ 1.0+2.0i 2.0+4.0i))
