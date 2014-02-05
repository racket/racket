#;#;
#<<END
TR opt: float-complex-div.rkt 2:0 (/ 1.0+2.0i 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-div.rkt 2:12 2.0+4.0i -- unboxed literal
TR opt: float-complex-div.rkt 2:21 3.0+6.0i -- unboxed literal
TR opt: float-complex-div.rkt 2:3 1.0+2.0i -- unboxed literal
END
#<<END
0.03333333333333333-0.06666666666666667i

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(/ 1.0+2.0i 2.0+4.0i 3.0+6.0i)
