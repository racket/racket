#;
(
TR opt: unboxed-letrec.rkt 22:0 (letrec-values (((f) (lambda (x) (#%app f x))) ((x) (quote 1.0+2.0i)) ((y) (#%app + (quote 2.0+4.0i) (quote 3.0+6.0i)))) (#%app + x y)) -- unboxed let bindings
TR opt: unboxed-letrec.rkt 23:31 1.0+2.0i -- unboxed literal
TR opt: unboxed-letrec.rkt 24:32 + -- unboxed binary float complex
TR opt: unboxed-letrec.rkt 24:34 2.0+4.0i -- unboxed literal
TR opt: unboxed-letrec.rkt 24:43 3.0+6.0i -- unboxed literal
TR opt: unboxed-letrec.rkt 25:2 (#%app + x y) -- unboxed float complex
TR opt: unboxed-letrec.rkt 25:3 + -- unboxed binary float complex
TR opt: unboxed-letrec.rkt 25:5 x -- leave var unboxed
TR opt: unboxed-letrec.rkt 25:5 x -- unbox float-complex
TR opt: unboxed-letrec.rkt 25:7 y -- leave var unboxed
TR opt: unboxed-letrec.rkt 25:7 y -- unbox float-complex
6.0+12.0i
)

#lang typed/scheme
#:optimize



(letrec: ((f : (Any -> Any) (lambda: ((x : Any)) (f x)))
          (x : Float-Complex   1.0+2.0i)
          (y : Float-Complex   (+ 2.0+4.0i 3.0+6.0i)))
  (+ x y))
