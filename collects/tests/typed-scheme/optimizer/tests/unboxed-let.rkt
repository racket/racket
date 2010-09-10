#;
(
unboxed-let.rkt line 38 col 14 - t1 - unbox inexact-complex
unboxed-let.rkt line 38 col 17 - 3.0+6.0i - unboxed literal
unboxed-let.rkt line 38 col 12 - - - unboxed binary inexact complex
unboxed-let.rkt line 38 col 11 - (#%app - t1 (quote 3.0+6.0i)) - unboxed inexact complex
unboxed-let.rkt line 37 col 14 - 1.0+2.0i - unboxed literal
unboxed-let.rkt line 37 col 23 - 2.0+4.0i - unboxed literal
unboxed-let.rkt line 37 col 12 - + - unboxed binary inexact complex
unboxed-let.rkt line 37 col 0 - (let-values (((t1) (#%app + (quote 1.0+2.0i) (quote 2.0+4.0i)))) (let-values (((t2) (#%app - t1 (quote 3.0+6.0i)))) (let-values (((t3) (quote 4.0+8.0i))) (#%app + t2 t3)))) - unboxed let bindings
unboxed-let.rkt line 40 col 5 - t2 - unbox inexact-complex
unboxed-let.rkt line 40 col 8 - t3 - unbox inexact-complex
unboxed-let.rkt line 40 col 3 - + - unboxed binary inexact complex
unboxed-let.rkt line 40 col 2 - (#%app + t2 t3) - unboxed inexact complex
unboxed-let.rkt line 38 col 14 - t1 - leave var unboxed
unboxed-let.rkt line 38 col 17 - 3.0+6.0i - unboxed literal
unboxed-let.rkt line 38 col 12 - - - unboxed binary inexact complex
unboxed-let.rkt line 37 col 0 - (let-values (((t2) (#%app - t1 (quote 3.0+6.0i)))) (let-values (((t3) (quote 4.0+8.0i))) (#%app + t2 t3))) - unboxed let bindings
unboxed-let.rkt line 40 col 5 - t2 - leave var unboxed
unboxed-let.rkt line 40 col 8 - t3 - unbox inexact-complex
unboxed-let.rkt line 40 col 3 - + - unboxed binary inexact complex
unboxed-let.rkt line 40 col 2 - (#%app + t2 t3) - unboxed inexact complex
unboxed-let.rkt line 39 col 11 - 4.0+8.0i - unboxed literal
unboxed-let.rkt line 37 col 0 - (let-values (((t3) (quote 4.0+8.0i))) (#%app + t2 t3)) - unboxed let bindings
unboxed-let.rkt line 40 col 5 - t2 - leave var unboxed
unboxed-let.rkt line 40 col 8 - t3 - leave var unboxed
unboxed-let.rkt line 40 col 3 - + - unboxed binary inexact complex
unboxed-let.rkt line 40 col 2 - (#%app + t2 t3) - unboxed inexact complex
4.0+8.0i
)

#lang typed/scheme
#:optimize



(let* ((t1 (+ 1.0+2.0i 2.0+4.0i))
       (t2 (- t1 3.0+6.0i))
       (t3 4.0+8.0i))
  (+ t2 t3))
