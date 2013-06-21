#;#;
#<<END
TR opt: maybe-exact-complex.rkt 18:3 1.0+2.0i -- unboxed literal
TR opt: maybe-exact-complex.rkt 18:12 2+4i -- unboxed literal
TR opt: maybe-exact-complex.rkt 18:0 (+ 1.0+2.0i 2+4i) -- unboxed binary float complex

END
#<<END
3.0+6.0i

END

#lang typed/scheme
#:optimize



(+ 1.0+2.0i 2+4i)
