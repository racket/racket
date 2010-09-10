#;
(
invalid-unboxed-let2.rkt line 25 col 33 - 1.0+2.0i - unboxed literal
invalid-unboxed-let2.rkt line 25 col 42 - 2.0+4.0i - unboxed literal
invalid-unboxed-let2.rkt line 25 col 31 - + - unboxed binary inexact complex
invalid-unboxed-let2.rkt line 25 col 30 - (#%app + (quote 1.0+2.0i) (quote 2.0+4.0i)) - unboxed inexact complex
invalid-unboxed-let2.rkt line 25 col 55 - 3.0+6.0i - unboxed literal
invalid-unboxed-let2.rkt line 25 col 64 - 4.0+8.0i - unboxed literal
invalid-unboxed-let2.rkt line 25 col 53 - + - unboxed binary inexact complex
invalid-unboxed-let2.rkt line 25 col 52 - (#%app + (quote 3.0+6.0i) (quote 4.0+8.0i)) - unboxed inexact complex
invalid-unboxed-let2.rkt line 25 col 0 - (let-values (((t1 t2) (#%app values (#%app + (quote 1.0+2.0i) (quote 2.0+4.0i)) (#%app + (quote 3.0+6.0i) (quote 4.0+8.0i))))) (#%app + t1 t2)) - unboxed let bindings
invalid-unboxed-let2.rkt line 26 col 5 - t1 - unbox inexact-complex
invalid-unboxed-let2.rkt line 26 col 8 - t2 - unbox inexact-complex
invalid-unboxed-let2.rkt line 26 col 3 - + - unboxed binary inexact complex
invalid-unboxed-let2.rkt line 26 col 2 - (#%app + t1 t2) - unboxed inexact complex
10.0+20.0i
)

#lang typed/scheme
#:optimize



;; unboxing of let bindings does not currently work with multiple values
(let-values (((t1 t2) (values (+ 1.0+2.0i 2.0+4.0i) (+ 3.0+6.0i 4.0+8.0i))))
  (+ t1 t2))
