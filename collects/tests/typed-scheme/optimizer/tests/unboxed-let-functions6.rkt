#;
(
TR opt: unboxed-let-functions6.rkt 26:0 (let: loop : Float-Complex ((z : Float-Complex 0.0+0.0i) (l : (Listof Integer) (quote (1 2 3)))) (if (null? l) (+ z 0.0+1.0i) (loop (+ z (car l)) (cdr l)))) -- unboxed call site
TR opt: unboxed-let-functions6.rkt 26:6 loop -- unboxed let loop
TR opt: unboxed-let-functions6.rkt 26:6 loop -- fun -> unboxed fun
TR opt: unboxed-let-functions6.rkt 26:6 loop -- unboxed function -> table
TR opt: unboxed-let-functions6.rkt 26:31 z -- unboxed var -> table
TR opt: unboxed-let-functions6.rkt 26:51 0.0+0.0i -- unboxed literal
TR opt: unboxed-let-functions6.rkt 29:10 (+ z 0.0+1.0i) -- unboxed binary float complex
TR opt: unboxed-let-functions6.rkt 29:13 z -- leave var unboxed
TR opt: unboxed-let-functions6.rkt 29:13 z -- unbox float-complex
TR opt: unboxed-let-functions6.rkt 29:15 0.0+1.0i -- unboxed literal
TR opt: unboxed-let-functions6.rkt 30:10 (loop (+ z (car l)) (cdr l)) -- call to fun with unboxed args
TR opt: unboxed-let-functions6.rkt 30:10 (loop (+ z (car l)) (cdr l)) -- unboxed call site
TR opt: unboxed-let-functions6.rkt 30:16 (+ z (car l)) -- unboxed binary float complex
TR opt: unboxed-let-functions6.rkt 30:19 z -- leave var unboxed
TR opt: unboxed-let-functions6.rkt 30:21 (car l) -- float-arg-expr in complex ops
TR opt: unboxed-let-functions6.rkt 30:21 (car l) -- pair
TR opt: unboxed-let-functions6.rkt 31:16 (cdr l) -- pair
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
