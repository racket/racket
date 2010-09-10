#;
(
invalid-unboxed-let.rkt line 38 col 14 - t1 - unbox inexact-complex
invalid-unboxed-let.rkt line 38 col 17 - t1 - unbox inexact-complex
invalid-unboxed-let.rkt line 38 col 12 - + - unboxed binary inexact complex
invalid-unboxed-let.rkt line 38 col 11 - (#%app + t1 t1) - unboxed inexact complex
invalid-unboxed-let.rkt line 38 col 14 - t1 - unbox inexact-complex
invalid-unboxed-let.rkt line 38 col 17 - t1 - unbox inexact-complex
invalid-unboxed-let.rkt line 38 col 12 - + - unboxed binary inexact complex
invalid-unboxed-let.rkt line 38 col 11 - (#%app + t1 t1) - unboxed inexact complex
invalid-unboxed-let.rkt line 38 col 14 - t1 - unbox inexact-complex
invalid-unboxed-let.rkt line 38 col 17 - t1 - unbox inexact-complex
invalid-unboxed-let.rkt line 38 col 12 - + - unboxed binary inexact complex
invalid-unboxed-let.rkt line 38 col 11 - (#%app + t1 t1) - unboxed inexact complex
invalid-unboxed-let.rkt line 34 col 13 - 1.0+2.0i - unboxed literal
invalid-unboxed-let.rkt line 34 col 22 - 2.0+4.0i - unboxed literal
invalid-unboxed-let.rkt line 34 col 11 - + - unboxed binary inexact complex
invalid-unboxed-let.rkt line 35 col 13 - 3.0+6.0i - unboxed literal
invalid-unboxed-let.rkt line 35 col 22 - 4.0+8.0i - unboxed literal
invalid-unboxed-let.rkt line 35 col 11 - + - unboxed binary inexact complex
invalid-unboxed-let.rkt line 35 col 10 - (#%app + (quote 3.0+6.0i) (quote 4.0+8.0i)) - unboxed inexact complex
invalid-unboxed-let.rkt line 34 col 0 - (let-values (((t1) (#%app + (quote 1.0+2.0i) (quote 2.0+4.0i))) ((t2) (#%app + (quote 3.0+6.0i) (quote 4.0+8.0i))) ((t3) (quote 1.0+2.0i)) ((t4) (quote 1))) (#%app display (#%app + t1 t1)) (#%app display t2) (#%app display t3) (#%app display t4)) - unboxed let bindings
invalid-unboxed-let.rkt line 38 col 14 - t1 - leave var unboxed
invalid-unboxed-let.rkt line 38 col 17 - t1 - leave var unboxed
invalid-unboxed-let.rkt line 38 col 12 - + - unboxed binary inexact complex
invalid-unboxed-let.rkt line 38 col 11 - (#%app + t1 t1) - unboxed inexact complex
6.0+12.0i7.0+14.0i1.0+2.0i1)

#lang typed/scheme
#:optimize



(let ((t1 (+ 1.0+2.0i 2.0+4.0i)) ; can be unboxed
      (t2 (+ 3.0+6.0i 4.0+8.0i)) ; can't be unboxed
      (t3 1.0+2.0i) ; can't be unboxed
      (t4 1))
  (display (+ t1 t1))
  (display t2)
  (display t3)
  (display t4))
