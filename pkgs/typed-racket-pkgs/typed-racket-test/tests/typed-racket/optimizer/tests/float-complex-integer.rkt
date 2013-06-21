#;#;
#<<END
TR opt: float-complex-integer.rkt 15:0 (+ (expt 2 100) 1.0+2.0i) -- unboxed binary float complex
TR opt: float-complex-integer.rkt 15:16 1.0+2.0i -- unboxed literal
TR opt: float-complex-integer.rkt 15:3 (expt 2 100) -- float-arg-expr in complex ops
END
#<<END
1.2676506002282294e+30+2.0i

END

#lang typed/scheme
#:optimize

(+ (expt 2 100) 1.0+2.0i)
