#;
(
unboxed-let3.rkt line 34 col 9 - x - unbox inexact-complex
unboxed-let3.rkt line 34 col 11 - 2.0+4.0i - unboxed literal
unboxed-let3.rkt line 34 col 7 - + - unboxed binary inexact complex
unboxed-let3.rkt line 34 col 6 - (#%app + x (quote 2.0+4.0i)) - unboxed inexact complex
unboxed-let3.rkt line 31 col 12 - 1.0+2.0i - unboxed literal
unboxed-let3.rkt line 31 col 21 - 2.0+4.0i - unboxed literal
unboxed-let3.rkt line 31 col 10 - + - unboxed binary inexact complex
unboxed-let3.rkt line 31 col 0 - (let-values (((x) (#%app + (quote 1.0+2.0i) (quote 2.0+4.0i)))) (if (#%app even? (quote 2)) x (#%app + x (quote 2.0+4.0i)))) - unboxed let bindings
unboxed-let3.rkt line 33 col 6 - x - unboxed complex variable
unboxed-let3.rkt line 34 col 9 - x - leave var unboxed
unboxed-let3.rkt line 34 col 11 - 2.0+4.0i - unboxed literal
unboxed-let3.rkt line 34 col 7 - + - unboxed binary inexact complex
unboxed-let3.rkt line 34 col 6 - (#%app + x (quote 2.0+4.0i)) - unboxed inexact complex
3.0+6.0i
)

#lang typed/scheme
#:optimize



;; both boxed and unboxed uses, we unbox anyway
;; causes unnecessary boxing/unboxing if we take a boxed path when
;; unboxing a complex literal or variable, but I expect this case
;; to be uncommon
;; by comparison, cases where we leave a result unboxed and box it
;; if needed (like here) or cases where this would unbox loop variables
;; are likely to be more common, and more interesting
(let ((x (+ 1.0+2.0i 2.0+4.0i)))
  (if (even? 2)
      x
      (+ x 2.0+4.0i)))
