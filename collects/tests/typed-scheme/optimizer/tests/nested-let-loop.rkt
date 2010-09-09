#;
(
nested-let-loop.rkt line 58 col 38 - r - unbox inexact-complex
nested-let-loop.rkt line 58 col 40 - s - unbox inexact-complex
nested-let-loop.rkt line 58 col 36 - + - unboxed binary inexact complex
nested-let-loop.rkt line 58 col 35 - (#%app + r s) - unboxed inexact complex
nested-let-loop.rkt line 51 col 8 - r - unboxed var -> table
nested-let-loop.rkt line 49 col 6 - loop1 - unboxed function -> table
nested-let-loop.rkt line 49 col 6 - loop1 - fun -> unboxed fun
nested-let-loop.rkt line 53 col 10 - r - unboxed complex variable
nested-let-loop.rkt line 58 col 38 - r - leave var unboxed
nested-let-loop.rkt line 58 col 40 - s - unbox inexact-complex
nested-let-loop.rkt line 58 col 36 - + - unboxed binary inexact complex
nested-let-loop.rkt line 58 col 35 - (#%app + r s) - unboxed inexact complex
nested-let-loop.rkt line 56 col 18 - s - unboxed var -> table
nested-let-loop.rkt line 54 col 16 - loop2 - unboxed function -> table
nested-let-loop.rkt line 54 col 16 - loop2 - fun -> unboxed fun
nested-let-loop.rkt line 58 col 38 - r - leave var unboxed
nested-let-loop.rkt line 58 col 40 - s - leave var unboxed
nested-let-loop.rkt line 58 col 36 - + - unboxed binary inexact complex
nested-let-loop.rkt line 58 col 21 - loop1 - unboxed call site
nested-let-loop.rkt line 58 col 28 - cdr - pair
nested-let-loop.rkt line 58 col 21 - loop1 - call to fun with unboxed args
nested-let-loop.rkt line 59 col 38 - s - leave var unboxed
nested-let-loop.rkt line 59 col 40 - (#%app car x) - unbox inexact-complex
nested-let-loop.rkt line 59 col 41 - car - pair
nested-let-loop.rkt line 59 col 48 - (#%app car y) - unbox inexact-complex
nested-let-loop.rkt line 59 col 49 - car - pair
nested-let-loop.rkt line 59 col 36 - + - unboxed binary inexact complex
nested-let-loop.rkt line 59 col 21 - loop2 - unboxed call site
nested-let-loop.rkt line 59 col 28 - cdr - pair
nested-let-loop.rkt line 59 col 21 - loop2 - call to fun with unboxed args
#f line #f col #f - (letrec-values (((loop2) (lambda (y s) (if (#%app null? y) (#%app loop1 (#%app cdr x) (#%app + r s)) (#%app loop2 (#%app cdr y) (#%app + s (#%app car x) (#%app car y))))))) loop2) - unboxed let bindings
nested-let-loop.rkt line 56 col 38 - 0.0+0.0i - unboxed literal
#f line #f col #f - (letrec-values (((loop2) (lambda (y s) (if (#%app null? y) (#%app loop1 (#%app cdr x) (#%app + r s)) (#%app loop2 (#%app cdr y) (#%app + s (#%app car x) (#%app car y))))))) loop2) - unboxed call site
nested-let-loop.rkt line 54 col 16 - loop2 - unboxed let loop
#f line #f col #f - (letrec-values (((loop1) (lambda (x r) (if (#%app null? x) r (#%app (letrec-values (((loop2) (lambda (y s) (if (#%app null? y) (#%app loop1 (#%app cdr x) (#%app + r s)) (#%app loop2 (#%app cdr y) (#%app + s (#%app car x) (#%app car y))))))) loop2) (quote (3.0+6.0i 4.0+8.0i)) (quote 0.0+0.0i)))))) loop1) - unboxed let bindings
nested-let-loop.rkt line 51 col 28 - 0.0+0.0i - unboxed literal
#f line #f col #f - (letrec-values (((loop1) (lambda (x r) (if (#%app null? x) r (#%app (letrec-values (((loop2) (lambda (y s) (if (#%app null? y) (#%app loop1 (#%app cdr x) (#%app + r s)) (#%app loop2 (#%app cdr y) (#%app + s (#%app car x) (#%app car y))))))) loop2) (quote (3.0+6.0i 4.0+8.0i)) (quote 0.0+0.0i)))))) loop1) - unboxed call site
nested-let-loop.rkt line 49 col 6 - loop1 - unboxed let loop
20.0+40.0i
)

#lang typed/scheme
#:optimize

(require racket/unsafe/ops)

(let: loop1 : Inexact-Complex
      ((x : (Listof Inexact-Complex) '(1.0+2.0i 2.0+4.0i))
       (r : Inexact-Complex 0.0+0.0i))
      (if (null? x)
          r
          (let: loop2 : Inexact-Complex
                ((y : (Listof Inexact-Complex) '(3.0+6.0i 4.0+8.0i))
                 (s : Inexact-Complex 0.0+0.0i))
                (if (null? y)
                    (loop1 (cdr x) (+ r s))
                    (loop2 (cdr y) (+ s (car x) (car y)))))))
