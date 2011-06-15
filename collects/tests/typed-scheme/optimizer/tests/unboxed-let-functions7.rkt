#;
(
TR opt: #f (no location) (letrec-values (((loop) (lambda (z l) (if (#%app null? l) z (#%app loop (#%app + z (#%app car l)) (#%app cdr l)))))) loop) -- unboxed call site
TR opt: unboxed-let-functions7.rkt 26:6 loop -- fun -> unboxed fun
TR opt: unboxed-let-functions7.rkt 26:6 loop -- unboxed function -> table
TR opt: unboxed-let-functions7.rkt 26:6 loop -- unboxed let loop
TR opt: unboxed-let-functions7.rkt 26:31 z -- unboxed var -> table
TR opt: unboxed-let-functions7.rkt 26:51 0.0+0.0i -- unboxed literal
TR opt: unboxed-let-functions7.rkt 29:6 z -- unboxed complex variable
TR opt: unboxed-let-functions7.rkt 30:7 loop -- call to fun with unboxed args
TR opt: unboxed-let-functions7.rkt 30:7 loop -- unboxed call site
TR opt: unboxed-let-functions7.rkt 30:12 (#%app + z (#%app car l)) -- unboxed float complex
TR opt: unboxed-let-functions7.rkt 30:13 + -- unboxed binary float complex
TR opt: unboxed-let-functions7.rkt 30:15 z -- leave var unboxed
TR opt: unboxed-let-functions7.rkt 30:15 z -- unbox float-complex
TR opt: unboxed-let-functions7.rkt 30:17 (#%app car l) -- float-arg-expr in complex ops
TR opt: unboxed-let-functions7.rkt 30:18 car -- pair
TR opt: unboxed-let-functions7.rkt 31:13 cdr -- pair
6.0+0.0i
)

#lang typed/scheme
#:optimize


(let: loop : Float-Complex   ((z : Float-Complex   0.0+0.0i)
                              (l : (Listof Integer) '(1 2 3)))
  (if (null? l)
      z ; boxed use. z should be unboxed anyway
      (loop (+ z (car l))
            (cdr l))))
