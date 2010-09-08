#;
(
nested-unboxed-let.rkt line 32 col 14 - x - unbox inexact-complex
nested-unboxed-let.rkt line 32 col 16 - 2.0+3.0i - unboxed literal
nested-unboxed-let.rkt line 32 col 12 - + - unboxed binary inexact complex
nested-unboxed-let.rkt line 32 col 11 - (#%app + x (quote 2.0+3.0i)) - unboxed inexact complex
nested-unboxed-let.rkt line 31 col 12 - 1.0+2.0i - unboxed literal
nested-unboxed-let.rkt line 31 col 21 - 2.0+3.0i - unboxed literal
nested-unboxed-let.rkt line 31 col 10 - + - unboxed binary inexact complex
nested-unboxed-let.rkt line 31 col 0 - (let-values (((x) (#%app + (quote 1.0+2.0i) (quote 2.0+3.0i)))) (let-values (((x) (#%app + x (quote 2.0+3.0i)))) (#%app + x (quote 3.0+6.0i)))) - unboxed let bindings
nested-unboxed-let.rkt line 33 col 7 - x - unbox inexact-complex
nested-unboxed-let.rkt line 33 col 9 - 3.0+6.0i - unboxed literal
nested-unboxed-let.rkt line 33 col 5 - + - unboxed binary inexact complex
nested-unboxed-let.rkt line 33 col 4 - (#%app + x (quote 3.0+6.0i)) - unboxed inexact complex
nested-unboxed-let.rkt line 32 col 14 - x - leave var unboxed
nested-unboxed-let.rkt line 32 col 16 - 2.0+3.0i - unboxed literal
nested-unboxed-let.rkt line 32 col 12 - + - unboxed binary inexact complex
nested-unboxed-let.rkt line 32 col 2 - (let-values (((x) (#%app + x (quote 2.0+3.0i)))) (#%app + x (quote 3.0+6.0i))) - unboxed let bindings
nested-unboxed-let.rkt line 33 col 7 - x - leave var unboxed
nested-unboxed-let.rkt line 33 col 9 - 3.0+6.0i - unboxed literal
nested-unboxed-let.rkt line 33 col 5 - + - unboxed binary inexact complex
nested-unboxed-let.rkt line 33 col 4 - (#%app + x (quote 3.0+6.0i)) - unboxed inexact complex
8.0+14.0i
)

#lang typed/scheme
#:optimize

(require racket/unsafe/ops)

(let ((x (+ 1.0+2.0i 2.0+3.0i)))
  (let ((x (+ x 2.0+3.0i)))
    (+ x 3.0+6.0i)))
