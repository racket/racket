#;
(
nested-let-loop.rkt 56:38 r -- unbox float-complex
nested-let-loop.rkt 56:40 s -- unbox float-complex
nested-let-loop.rkt 56:36 + -- unboxed binary float complex
nested-let-loop.rkt 56:35 (#%app + r s) -- unboxed float complex
nested-let-loop.rkt 49:8 r -- unboxed var -> table
nested-let-loop.rkt 47:6 loop1 -- unboxed function -> table
nested-let-loop.rkt 47:6 loop1 -- fun -> unboxed fun
nested-let-loop.rkt 51:10 r -- unboxed complex variable
nested-let-loop.rkt 56:38 r -- leave var unboxed
nested-let-loop.rkt 56:40 s -- unbox float-complex
nested-let-loop.rkt 56:36 + -- unboxed binary float complex
nested-let-loop.rkt 56:35 (#%app + r s) -- unboxed float complex
nested-let-loop.rkt 54:18 s -- unboxed var -> table
nested-let-loop.rkt 52:16 loop2 -- unboxed function -> table
nested-let-loop.rkt 52:16 loop2 -- fun -> unboxed fun
nested-let-loop.rkt 56:38 r -- leave var unboxed
nested-let-loop.rkt 56:40 s -- leave var unboxed
nested-let-loop.rkt 56:36 + -- unboxed binary float complex
nested-let-loop.rkt 56:21 loop1 -- unboxed call site
nested-let-loop.rkt 56:28 cdr -- pair
nested-let-loop.rkt 56:21 loop1 -- call to fun with unboxed args
nested-let-loop.rkt 57:38 s -- leave var unboxed
nested-let-loop.rkt 57:40 (#%app car x) -- unbox float-complex
nested-let-loop.rkt 57:41 car -- pair
nested-let-loop.rkt 57:48 (#%app car y) -- unbox float-complex
nested-let-loop.rkt 57:49 car -- pair
nested-let-loop.rkt 57:36 + -- unboxed binary float complex
nested-let-loop.rkt 57:21 loop2 -- unboxed call site
nested-let-loop.rkt 57:28 cdr -- pair
nested-let-loop.rkt 57:21 loop2 -- call to fun with unboxed args
nested-let-loop.rkt 54:38 0.0+0.0i -- unboxed literal
#f (no location) (letrec-values (((loop2) (lambda (y s) (if (#%app null? y) (#%app loop1 (#%app cdr x) (#%app + r s)) (#%app loop2 (#%app cdr y) (#%app + s (#%app car x) (#%app car y))))))) loop2) -- unboxed call site
nested-let-loop.rkt 52:16 loop2 -- unboxed let loop
nested-let-loop.rkt 49:28 0.0+0.0i -- unboxed literal
#f (no location) (letrec-values (((loop1) (lambda (x r) (if (#%app null? x) r (#%app (letrec-values (((loop2) (lambda (y s) (if (#%app null? y) (#%app loop1 (#%app cdr x) (#%app + r s)) (#%app loop2 (#%app cdr y) (#%app + s (#%app car x) (#%app car y))))))) loop2) (quote (3.0+6.0i 4.0+8.0i)) (quote 0.0+0.0i)))))) loop1) -- unboxed call site
nested-let-loop.rkt 47:6 loop1 -- unboxed let loop
20.0+40.0i
)

#lang typed/scheme
#:optimize



(let: loop1 : Float-Complex
      ((x : (Listof Float-Complex)   '(1.0+2.0i 2.0+4.0i))
       (r : Float-Complex   0.0+0.0i))
      (if (null? x)
          r
          (let: loop2 : Float-Complex
                ((y : (Listof Float-Complex)   '(3.0+6.0i 4.0+8.0i))
                 (s : Float-Complex   0.0+0.0i))
                (if (null? y)
                    (loop1 (cdr x) (+ r s))
                    (loop2 (cdr y) (+ s (car x) (car y)))))))
