#lang racket

(require racket/unsafe/ops)

;; both boxed and unboxed uses, we unbox anyway
;; causes unnecessary boxing/unboxing if we take a boxed path when
;; unboxing a complex literal or variable, but I expect this case
;; to be uncommon
;; by comparison, cases where we leave a result unboxed and box it
;; if needed (like here) or cases where this would unbox loop variables
;; are likely to be more common, and more interesting
(let*-values (((unboxed-gensym-1) 1.0)
              ((unboxed-gensym-2) 2.0)
              ((unboxed-gensym-3) 2.0)
              ((unboxed-gensym-4) 4.0)
              ((unboxed-gensym-5) (unsafe-fl+ unboxed-gensym-1 unboxed-gensym-3))
              ((unboxed-gensym-6) (unsafe-fl+ unboxed-gensym-2 unboxed-gensym-4)))
  (if (even? 2)
      (unsafe-make-flrectangular unboxed-gensym-5 unboxed-gensym-6)
      (let*-values (((unboxed-gensym-7) 2.0)
                    ((unboxed-gensym-8) 4.0)
                    ((unboxed-gensym-9) (unsafe-fl+ unboxed-gensym-5 unboxed-gensym-7))
                    ((unboxed-gensym-10) (unsafe-fl+ unboxed-gensym-6 unboxed-gensym-8)))
        (unsafe-make-flrectangular unboxed-gensym-9 unboxed-gensym-10))))
(void)
