#lang racket

(require racket/unsafe/ops)

;; both boxed and unboxed uses, we unbox anyway
;; causes unnecessary boxing/unboxing if we take a boxed path when
;; unboxing a complex literal or variable, but I expect this case
;; to be uncommon
;; by comparison, cases where we leave a result unboxed and box it
;; if needed (like here) or cases where this would unbox loop variables
;; are likely to be more common, and more interesting
(let*-values (((unboxed-real-1) 1.0)
              ((unboxed-imag-2) 2.0)
              ((unboxed-real-3) 2.0)
              ((unboxed-imag-4) 4.0)
              ((unboxed-real-5) (unsafe-fl+ unboxed-real-1 unboxed-real-3))
              ((unboxed-imag-6) (unsafe-fl+ unboxed-imag-2 unboxed-imag-4)))
  (if (even? 2)
      (unsafe-make-flrectangular unboxed-real-5 unboxed-imag-6)
      (let*-values (((unboxed-real-7) 2.0)
                    ((unboxed-imag-8) 4.0)
                    ((unboxed-real-9) (unsafe-fl+ unboxed-real-5 unboxed-real-7))
                    ((unboxed-imag-10) (unsafe-fl+ unboxed-imag-6 unboxed-imag-8)))
        (unsafe-make-flrectangular unboxed-real-9 unboxed-imag-10))))
(void)
