#;
(
unboxed-let-functions6.rkt 35:13 z -- unbox float-complex
unboxed-let-functions6.rkt 35:15 0.0+1.0i -- unboxed literal
unboxed-let-functions6.rkt 35:11 + -- unboxed binary float complex
unboxed-let-functions6.rkt 35:10 (#%app + z (quote 0.0+1.0i)) -- unboxed float complex
unboxed-let-functions6.rkt 32:31 z -- unboxed var -> table
unboxed-let-functions6.rkt 32:6 loop -- unboxed function -> table
unboxed-let-functions6.rkt 32:6 loop -- fun -> unboxed fun
unboxed-let-functions6.rkt 35:13 z -- leave var unboxed
unboxed-let-functions6.rkt 35:15 0.0+1.0i -- unboxed literal
unboxed-let-functions6.rkt 35:11 + -- unboxed binary float complex
unboxed-let-functions6.rkt 35:10 (#%app + z (quote 0.0+1.0i)) -- unboxed float complex
unboxed-let-functions6.rkt 36:19 z -- leave var unboxed
unboxed-let-functions6.rkt 36:22 car -- pair
unboxed-let-functions6.rkt 36:21 (#%app car l) -- float-coerce-expr in complex ops
unboxed-let-functions6.rkt 36:17 + -- unboxed binary float complex
unboxed-let-functions6.rkt 36:11 loop -- unboxed call site
unboxed-let-functions6.rkt 37:17 cdr -- pair
unboxed-let-functions6.rkt 36:11 loop -- call to fun with unboxed args
unboxed-let-functions6.rkt 32:51 0.0+0.0i -- unboxed literal
#f (no location) (letrec-values (((loop) (lambda (z l) (if (#%app null? l) (#%app + z (quote 0.0+1.0i)) (#%app loop (#%app + z (#%app car l)) (#%app cdr l)))))) loop) -- unboxed call site
unboxed-let-functions6.rkt 32:6 loop -- unboxed let loop
6.0+1.0i
)

#lang typed/scheme
#:optimize



(let: loop :   Float-Complex ((z : Float-Complex   0.0+0.0i)
                              (l : (Listof Integer) '(1 2 3)))
      (if (null? l)
          (+ z 0.0+1.0i)
          (loop (+ z (car l))
                (cdr l))))
