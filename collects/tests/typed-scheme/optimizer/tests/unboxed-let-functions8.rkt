#;
(
unboxed-let-functions8.rkt line 16 col 67 - x - unbox inexact-complex
unboxed-let-functions8.rkt line 16 col 69 - 2.0+4.0i - unboxed literal
unboxed-let-functions8.rkt line 16 col 65 - + - unboxed binary inexact complex
unboxed-let-functions8.rkt line 16 col 64 - (#%app + x (quote 2.0+4.0i)) - unboxed inexact complex
unboxed-let-functions8.rkt line 16 col 0 - (letrec-values (((f) (lambda (x) (#%app + x (quote 2.0+4.0i)))) ((g) f)) (#%app f (quote 1.0+2.0i))) - unboxed let bindings
3.0+6.0i
)

#lang typed/scheme
#:optimize



(letrec: ((f : (Inexact-Complex -> Inexact-Complex) (lambda (x) (+ x 2.0+4.0i)))
          (g : (Inexact-Complex -> Inexact-Complex) f)) ; f escapes! can't unbox it's args
  (f 1.0+2.0i))
