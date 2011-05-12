#;
(
nested-unboxed-let.rkt 32:14 x -- unbox float-complex
nested-unboxed-let.rkt 32:16 2.0+3.0i -- unboxed literal
nested-unboxed-let.rkt 32:12 + -- unboxed binary float complex
nested-unboxed-let.rkt 32:11 (#%app + x (quote 2.0+3.0i)) -- unboxed float complex
nested-unboxed-let.rkt 31:12 1.0+2.0i -- unboxed literal
nested-unboxed-let.rkt 31:21 2.0+3.0i -- unboxed literal
nested-unboxed-let.rkt 31:10 + -- unboxed binary float complex
nested-unboxed-let.rkt 31:0 (let-values (((x) (#%app + (quote 1.0+2.0i) (quote 2.0+3.0i)))) (let-values (((x) (#%app + x (quote 2.0+3.0i)))) (#%app + x (quote 3.0+6.0i)))) -- unboxed let bindings
nested-unboxed-let.rkt 33:7 x -- unbox float-complex
nested-unboxed-let.rkt 33:9 3.0+6.0i -- unboxed literal
nested-unboxed-let.rkt 33:5 + -- unboxed binary float complex
nested-unboxed-let.rkt 33:4 (#%app + x (quote 3.0+6.0i)) -- unboxed float complex
nested-unboxed-let.rkt 32:14 x -- leave var unboxed
nested-unboxed-let.rkt 32:16 2.0+3.0i -- unboxed literal
nested-unboxed-let.rkt 32:12 + -- unboxed binary float complex
nested-unboxed-let.rkt 32:2 (let-values (((x) (#%app + x (quote 2.0+3.0i)))) (#%app + x (quote 3.0+6.0i))) -- unboxed let bindings
nested-unboxed-let.rkt 33:7 x -- leave var unboxed
nested-unboxed-let.rkt 33:9 3.0+6.0i -- unboxed literal
nested-unboxed-let.rkt 33:5 + -- unboxed binary float complex
nested-unboxed-let.rkt 33:4 (#%app + x (quote 3.0+6.0i)) -- unboxed float complex
8.0+14.0i
)

#lang typed/scheme
#:optimize



(let ((x (+ 1.0+2.0i 2.0+3.0i)))
  (let ((x (+ x 2.0+3.0i)))
    (+ x 3.0+6.0i)))
