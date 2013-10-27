#;#;
#<<END
TR opt: float-complex-unary.rkt 31:0 (real-part (+ 1.0+2.0i (+ (* 3.0+4.0i 5.0+6.0i)))) -- complex accessor elimination
TR opt: float-complex-unary.rkt 31:11 (+ 1.0+2.0i (+ (* 3.0+4.0i 5.0+6.0i))) -- unboxed binary float complex
TR opt: float-complex-unary.rkt 31:14 1.0+2.0i -- unboxed literal
TR opt: float-complex-unary.rkt 31:23 (+ (* 3.0+4.0i 5.0+6.0i)) -- unboxed unary float complex
TR opt: float-complex-unary.rkt 31:26 (* 3.0+4.0i 5.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-unary.rkt 31:26 (* 3.0+4.0i 5.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-unary.rkt 31:29 3.0+4.0i -- unboxed literal
TR opt: float-complex-unary.rkt 31:29 3.0+4.0i -- unboxed literal
TR opt: float-complex-unary.rkt 31:38 5.0+6.0i -- unboxed literal
TR opt: float-complex-unary.rkt 31:38 5.0+6.0i -- unboxed literal
TR opt: float-complex-unary.rkt 32:0 (real-part (+ 7.0+8.0i (* (+ 9.0+10.0i 11.0+12.0i)))) -- complex accessor elimination
TR opt: float-complex-unary.rkt 32:11 (+ 7.0+8.0i (* (+ 9.0+10.0i 11.0+12.0i))) -- unboxed binary float complex
TR opt: float-complex-unary.rkt 32:14 7.0+8.0i -- unboxed literal
TR opt: float-complex-unary.rkt 32:23 (* (+ 9.0+10.0i 11.0+12.0i)) -- unboxed unary float complex
TR opt: float-complex-unary.rkt 32:26 (+ 9.0+10.0i 11.0+12.0i) -- unboxed binary float complex
TR opt: float-complex-unary.rkt 32:26 (+ 9.0+10.0i 11.0+12.0i) -- unboxed binary float complex
TR opt: float-complex-unary.rkt 32:29 9.0+10.0i -- unboxed literal
TR opt: float-complex-unary.rkt 32:29 9.0+10.0i -- unboxed literal
TR opt: float-complex-unary.rkt 32:39 11.0+12.0i -- unboxed literal
TR opt: float-complex-unary.rkt 32:39 11.0+12.0i -- unboxed literal
END
#<<END
-8.0
27.0

END
#lang typed/racket

(real-part (+ 1.0+2.0i (+ (* 3.0+4.0i 5.0+6.0i))))
(real-part (+ 7.0+8.0i (* (+ 9.0+10.0i 11.0+12.0i))))

