#lang racket

(require racket/unsafe/ops)

;; top level
(let*-values (((unboxed-gensym-1) 1.0)
              ((unboxed-gensym-2) 1.0)
              ((unboxed-gensym-3) (unsafe-fl* unboxed-gensym-1
                                              (unsafe-flcos unboxed-gensym-2)))
              ((unboxed-gensym-4) (unsafe-fl* unboxed-gensym-1
                                              (unsafe-flsin unboxed-gensym-2))))
  (unsafe-make-flrectangular unboxed-gensym-3 unboxed-gensym-4))

;; nested
(let*-values (((unboxed-gensym-1) 1.0+2.0i)
              ((unboxed-gensym-2) (unsafe-flreal-part unboxed-gensym-1))
              ((unboxed-gensym-3) (unsafe-flimag-part unboxed-gensym-1))
              ((unboxed-gensym-4) 2.0)
              ((unboxed-gensym-5) 4.0)
              ((unboxed-gensym-6) (unsafe-fl* unboxed-gensym-4
                                              (unsafe-flcos unboxed-gensym-5)))
              ((unboxed-gensym-7) (unsafe-fl* unboxed-gensym-4
                                              (unsafe-flsin unboxed-gensym-5)))
              ((unboxed-gensym-8) (unsafe-fl+ unboxed-gensym-2 unboxed-gensym-6))
              ((unboxed-gensym-9) (unsafe-fl+ unboxed-gensym-3 unboxed-gensym-7)))
  (unsafe-make-flrectangular unboxed-gensym-8 unboxed-gensym-9))

(void)
