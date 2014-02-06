#;#;
#<<END
TR opt: unboxed-letrec.rkt 5:10 (x : Float-Complex 1.0+2.0i) -- unboxed let bindings
TR opt: unboxed-letrec.rkt 5:31 1.0+2.0i -- unboxed literal
TR opt: unboxed-letrec.rkt 6:10 (y : Float-Complex (+ 2.0+4.0i 3.0+6.0i)) -- unboxed let bindings
TR opt: unboxed-letrec.rkt 6:31 (+ 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: unboxed-letrec.rkt 6:34 2.0+4.0i -- unboxed literal
TR opt: unboxed-letrec.rkt 6:43 3.0+6.0i -- unboxed literal
TR opt: unboxed-letrec.rkt 7:2 (+ x y) -- unboxed binary float complex
TR opt: unboxed-letrec.rkt 7:5 x -- leave var unboxed
TR opt: unboxed-letrec.rkt 7:7 y -- leave var unboxed
END
#<<END
6.0+12.0i

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port



(letrec: ((f : (Any -> Any) (lambda: ((x : Any)) (f x)))
          (x : Float-Complex   1.0+2.0i)
          (y : Float-Complex   (+ 2.0+4.0i 3.0+6.0i)))
  (+ x y))
