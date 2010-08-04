#lang racket

(require racket/unsafe/ops)

;; function with a mix of complex and non-complex args
(let ((f (lambda (unboxed-real-1 unboxed-imag-2 y)
           (let*-values (((unboxed-gensym-3) y)
                         ((unboxed-gensym-4) (unsafe-fl+ unboxed-real-1 unboxed-gensym-3))
                         ((unboxed-gensym-5) unboxed-imag-2))
             (unsafe-make-flrectangular unboxed-gensym-4 unboxed-gensym-5)))))
  (let*-values (((unboxed-gensym-1) 1.0)
                ((unboxed-gensym-2) 2.0)
                ((unboxed-gensym-3) 2.0)
                ((unboxed-gensym-4) 4.0)
                ((unboxed-gensym-5) (unsafe-fl+ unboxed-gensym-1 unboxed-gensym-3))
                ((unboxed-gensym-6) (unsafe-fl+ unboxed-gensym-2 unboxed-gensym-4)))
    (f unboxed-gensym-5 unboxed-gensym-6 3.0)))
(void)
