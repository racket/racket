#;#;
#<<END
TR opt: float-complex.rkt 19:0 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex.rkt 19:12 2.0+4.0i -- unboxed literal
TR opt: float-complex.rkt 19:3 1.0+2.0i -- unboxed literal
TR opt: float-complex.rkt 20:0 (- 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex.rkt 20:12 2.0+4.0i -- unboxed literal
TR opt: float-complex.rkt 20:3 1.0+2.0i -- unboxed literal
END
#<<END
3.0+6.0i
-1.0-2.0i

END

#lang typed/scheme
#:optimize

(+ 1.0+2.0i 2.0+4.0i)
(- 1.0+2.0i 2.0+4.0i)
