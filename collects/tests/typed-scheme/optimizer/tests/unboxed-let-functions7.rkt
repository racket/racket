#;
(
unboxed-let-functions7.rkt line 34 col 15 - z - unbox float-complex
unboxed-let-functions7.rkt line 34 col 18 - car - pair
unboxed-let-functions7.rkt line 34 col 17 - (#%app car l) - float-coerce-expr in complex ops
unboxed-let-functions7.rkt line 34 col 13 - + - unboxed binary float complex
unboxed-let-functions7.rkt line 34 col 12 - (#%app + z (#%app car l)) - unboxed float complex
unboxed-let-functions7.rkt line 30 col 31 - z - unboxed var -> table
unboxed-let-functions7.rkt line 30 col 6 - loop - unboxed function -> table
unboxed-let-functions7.rkt line 30 col 6 - loop - fun -> unboxed fun
unboxed-let-functions7.rkt line 33 col 6 - z - unboxed complex variable
unboxed-let-functions7.rkt line 34 col 15 - z - leave var unboxed
unboxed-let-functions7.rkt line 34 col 18 - car - pair
unboxed-let-functions7.rkt line 34 col 17 - (#%app car l) - float-coerce-expr in complex ops
unboxed-let-functions7.rkt line 34 col 13 - + - unboxed binary float complex
unboxed-let-functions7.rkt line 34 col 7 - loop - unboxed call site
unboxed-let-functions7.rkt line 35 col 13 - cdr - pair
unboxed-let-functions7.rkt line 34 col 7 - loop - call to fun with unboxed args
unboxed-let-functions7.rkt line 30 col 51 - 0.0+0.0i - unboxed literal
#f line #f col #f - (letrec-values (((loop) (lambda (z l) (if (#%app null? l) z (#%app loop (#%app + z (#%app car l)) (#%app cdr l)))))) loop) - unboxed call site
unboxed-let-functions7.rkt line 30 col 6 - loop - unboxed let loop
6.0+0.0i
)

#lang typed/scheme
#:optimize



(let: loop : Inexact-Complex ((z : Inexact-Complex 0.0+0.0i)
                              (l : (Listof Integer) '(1 2 3)))
  (if (null? l)
      z ; boxed use. z should be unboxed anyway
      (loop (+ z (car l))
            (cdr l))))
