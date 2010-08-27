#lang typed/scheme
#:optimize

(require racket/unsafe/ops)

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
