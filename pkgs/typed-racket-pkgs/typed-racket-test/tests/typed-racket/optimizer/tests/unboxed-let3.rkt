#;#;
#<<END
TR opt: unboxed-let3.rkt 11:6 x -- unboxed complex variable
TR opt: unboxed-let3.rkt 12:11 2.0+4.0i -- unboxed literal
TR opt: unboxed-let3.rkt 12:6 (+ x 2.0+4.0i) -- unboxed binary float complex
TR opt: unboxed-let3.rkt 12:9 x -- leave var unboxed
TR opt: unboxed-let3.rkt 9:12 1.0+2.0i -- unboxed literal
TR opt: unboxed-let3.rkt 9:21 2.0+4.0i -- unboxed literal
TR opt: unboxed-let3.rkt 9:6 (x (+ 1.0+2.0i 2.0+4.0i)) -- unboxed let bindings
TR opt: unboxed-let3.rkt 9:9 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
END
#<<END
3.0+6.0i

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

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
