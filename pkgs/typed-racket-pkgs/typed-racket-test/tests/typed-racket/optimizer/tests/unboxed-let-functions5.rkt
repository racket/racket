#;#;
#<<END
TR opt: unboxed-let-functions5.rkt 9:12 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: unboxed-let-functions5.rkt 9:15 1.0+2.0i -- unboxed literal
TR opt: unboxed-let-functions5.rkt 9:24 2.0+4.0i -- unboxed literal
END
#<<END
3.0+6.0i

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port



;; invalid: f "escapes", according to our analysis
(letrec: ((f : (Float-Complex -> Float-Complex)
             (lambda: ((x : Float-Complex))
                      (let: ((y : (Float-Complex -> Float-Complex) f))
                            x))))
         (f (+ 1.0+2.0i 2.0+4.0i)))
