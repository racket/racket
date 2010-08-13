#lang racket

(require racket/unsafe/ops)

;; top level
(let*-values (((unboxed-gensym-1) 1.0)
              ((unboxed-gensym-2) 1.0)
              ((unboxed-real-3) (unsafe-fl* unboxed-gensym-1
                                            (unsafe-flcos unboxed-gensym-2)))
              ((unboxed-imag-4) (unsafe-fl* unboxed-gensym-1
                                            (unsafe-flsin unboxed-gensym-2))))
  (unsafe-make-flrectangular unboxed-real-3 unboxed-imag-4))

;; nested
(let*-values (((unboxed-real-1) 1.0)
              ((unboxed-imag-2) 2.0)
              ((unboxed-gensym-3) 2.0)
              ((unboxed-gensym-4) 4.0)
              ((unboxed-real-5) (unsafe-fl* unboxed-gensym-3
                                            (unsafe-flcos unboxed-gensym-4)))
              ((unboxed-imag-6) (unsafe-fl* unboxed-gensym-3
                                            (unsafe-flsin unboxed-gensym-4)))
              ((unboxed-real-7) (unsafe-fl+ unboxed-real-1 unboxed-real-5))
              ((unboxed-imag-8) (unsafe-fl+ unboxed-imag-2 unboxed-imag-6)))
  (unsafe-make-flrectangular unboxed-real-7 unboxed-imag-8))

(void)
