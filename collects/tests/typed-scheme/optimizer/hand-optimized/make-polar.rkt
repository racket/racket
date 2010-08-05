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
(let*-values (((unboxed-gensym-1) 1.0)
              ((unboxed-gensym-2) 2.0)
              ((unboxed-gensym-3) 2.0)
              ((unboxed-gensym-4) 4.0)
              ((unboxed-gensym-5) (unsafe-fl* unboxed-gensym-3
                                              (unsafe-flcos unboxed-gensym-4)))
              ((unboxed-gensym-6) (unsafe-fl* unboxed-gensym-3
                                              (unsafe-flsin unboxed-gensym-4)))
              ((unboxed-gensym-7) (unsafe-fl+ unboxed-gensym-1 unboxed-gensym-5))
              ((unboxed-gensym-8) (unsafe-fl+ unboxed-gensym-2 unboxed-gensym-6)))
  (unsafe-make-flrectangular unboxed-gensym-7 unboxed-gensym-8))

(void)
