#;
(
float-complex-i.rkt 15:3 1.0+2.0i -- unboxed literal
float-complex-i.rkt 15:15 0+1.0i -- unboxed literal
float-complex-i.rkt 15:21 2.0+4.0i -- unboxed literal
float-complex-i.rkt 15:13 * -- unboxed binary float complex
float-complex-i.rkt 15:1 + -- unboxed binary float complex
float-complex-i.rkt 15:0 (#%app + (quote 1.0+2.0i) (#%app * (quote 0+1.0i) (quote 2.0+4.0i))) -- unboxed float complex
-3.0+4.0i
)

#lang typed/scheme
#:optimize

(+ 1.0+2.0i (* +1.0i 2.0+4.0i))
