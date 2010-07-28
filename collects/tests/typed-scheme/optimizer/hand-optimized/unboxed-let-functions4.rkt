#lang racket

(require racket/unsafe/ops)

;; function with a mix of complex and non-complex args
(let ((f (lambda (unboxed-real-1 unboxed-imag-2 y)
           (let*-values (((unboxed-gensym-3) y)
                         ((unboxed-gensym-4) (unsafe-fl+ unboxed-real-1 unboxed-gensym-3))
                         ((unboxed-gensym-5) unboxed-imag-2))
             (unsafe-make-flrectangular unboxed-gensym-4 unboxed-gensym-5)))))
  (let*-values (((unboxed-gensym-1) 1.0+2.0i)
                ((unboxed-gensym-2) (unsafe-flreal-part unboxed-gensym-1))
                ((unboxed-gensym-3) (unsafe-flimag-part unboxed-gensym-1))
                ((unboxed-gensym-4) 2.0+4.0i)
                ((unboxed-gensym-5) (unsafe-flreal-part unboxed-gensym-4))
                ((unboxed-gensym-6) (unsafe-flimag-part unboxed-gensym-4))
                ((unboxed-gensym-7) (unsafe-fl+ unboxed-gensym-2 unboxed-gensym-5))
                ((unboxed-gensym-8) (unsafe-fl+ unboxed-gensym-3 unboxed-gensym-6)))
    (f unboxed-gensym-7 unboxed-gensym-8 3.0)))
(void)
