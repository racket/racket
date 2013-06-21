#;#;
#<<END
TR opt: nested-float-complex.rkt 17:0 (+ 1.0+2.0i (- 2.0+4.0i 3.0+6.0i)) -- unboxed binary float complex
TR opt: nested-float-complex.rkt 17:12 (- 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: nested-float-complex.rkt 17:15 2.0+4.0i -- unboxed literal
TR opt: nested-float-complex.rkt 17:24 3.0+6.0i -- unboxed literal
TR opt: nested-float-complex.rkt 17:3 1.0+2.0i -- unboxed literal
END
#<<END
0.0+0.0i

END

#lang typed/scheme
#:optimize

(+ 1.0+2.0i (- 2.0+4.0i 3.0+6.0i))
