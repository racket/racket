#lang racket

(require racket/unsafe/ops)

;; both boxed and unboxed uses, we unbox anyway
;; causes unnecessary boxing/unboxing if we take a boxed path when
;; unboxing a complex literal or variable, but I expect this case
;; to be uncommon
;; by comparison, cases where we leave a result unboxed and box it
;; if needed (like here) or cases where this would unbox loop variables
;; are likely to be more common, and more interesting
(let*-values (((unboxed-gensym-1) 1.0+2.0i)
              ((unboxed-gensym-2) (unsafe-flreal-part unboxed-gensym-1))
              ((unboxed-gensym-3) (unsafe-flimag-part unboxed-gensym-1))
              ((unboxed-gensym-4) 2.0+4.0i)
              ((unboxed-gensym-5) (unsafe-flreal-part unboxed-gensym-4))
              ((unboxed-gensym-6) (unsafe-flimag-part unboxed-gensym-4))
              ((unboxed-gensym-7) (unsafe-fl+ unboxed-gensym-2 unboxed-gensym-5))
              ((unboxed-gensym-8) (unsafe-fl+ unboxed-gensym-3 unboxed-gensym-6)))
  (if (even? 2)
      (unsafe-make-flrectangular unboxed-gensym-7 unboxed-gensym-8)
      (let*-values (((unboxed-gensym-9) 2.0+4.0i)
                    ((unboxed-gensym-10) (unsafe-flreal-part unboxed-gensym-9))
                    ((unboxed-gensym-11) (unsafe-flimag-part unboxed-gensym-9))
                    ((unboxed-gensym-12) (unsafe-fl+ unboxed-gensym-7 unboxed-gensym-10))
                    ((unboxed-gensym-13) (unsafe-fl+ unboxed-gensym-8 unboxed-gensym-11)))
        (unsafe-make-flrectangular unboxed-gensym-12 unboxed-gensym-13))))
(void)
