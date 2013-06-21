#;#;
#<<END
TR opt: float-complex-mult.rkt 16:0 (* 1.0+2.0i 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-mult.rkt 16:12 2.0+4.0i -- unboxed literal
TR opt: float-complex-mult.rkt 16:21 3.0+6.0i -- unboxed literal
TR opt: float-complex-mult.rkt 16:3 1.0+2.0i -- unboxed literal
END
#<<END
-66.0-12.0i

END

#lang typed/scheme
#:optimize

(* 1.0+2.0i 2.0+4.0i 3.0+6.0i)
