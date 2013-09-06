#;#;
#<<END
TR opt: unboxed-let-functions7.rkt 26:0 (let: loop : Float-Complex ((z : Float-Complex 0.0+0.0i) (l : (Listof Integer) (quote (1 2 3)))) (if (null? l) z (loop (+ z (car l)) (cdr l)))) -- unboxed call site
TR opt: unboxed-let-functions7.rkt 26:31 z -- unboxed var -> table
TR opt: unboxed-let-functions7.rkt 26:51 0.0+0.0i -- unboxed literal
TR opt: unboxed-let-functions7.rkt 26:6 loop -- fun -> unboxed fun
TR opt: unboxed-let-functions7.rkt 26:6 loop -- unboxed let loop
TR opt: unboxed-let-functions7.rkt 29:6 z -- unboxed complex variable
TR opt: unboxed-let-functions7.rkt 30:12 (+ z (car l)) -- unboxed binary float complex
TR opt: unboxed-let-functions7.rkt 30:15 z -- leave var unboxed
TR opt: unboxed-let-functions7.rkt 30:17 (car l) -- float-arg-expr in complex ops
TR opt: unboxed-let-functions7.rkt 30:17 (car l) -- pair
TR opt: unboxed-let-functions7.rkt 30:6 (loop (+ z (car l)) (cdr l)) -- call to fun with unboxed args
TR opt: unboxed-let-functions7.rkt 30:6 (loop (+ z (car l)) (cdr l)) -- unboxed call site
TR opt: unboxed-let-functions7.rkt 31:12 (cdr l) -- pair
END
#<<END
6.0+0.0i

END

#lang typed/scheme
#:optimize


(let: loop : Float-Complex   ((z : Float-Complex   0.0+0.0i)
                              (l : (Listof Integer) '(1 2 3)))
  (if (null? l)
      z ; boxed use. z should be unboxed anyway
      (loop (+ z (car l))
            (cdr l))))
