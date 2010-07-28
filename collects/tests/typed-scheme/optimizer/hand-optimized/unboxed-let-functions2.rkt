#lang racket

(require racket/unsafe/ops)

;; function with multiple complex args
(let ((f (lambda (unboxed-real-1 unboxed-real-2 unboxed-imag-3 unboxed-imag-4)
           (let*-values (((unboxed-gensym-5) (unsafe-fl+ unboxed-real-1 unboxed-real-2))
                         ((unboxed-gensym-6) (unsafe-fl+ unboxed-imag-3 unboxed-imag-4)))
             (unsafe-make-flrectangular unboxed-gensym-5 unboxed-gensym-6)))))
  (let*-values (((unboxed-gensym-1) 1.0+2.0i)
                ((unboxed-gensym-2) (unsafe-flreal-part unboxed-gensym-1))
                ((unboxed-gensym-3) (unsafe-flimag-part unboxed-gensym-1))
                ((unboxed-gensym-4) 2.0+4.0i)
                ((unboxed-gensym-5) (unsafe-flreal-part unboxed-gensym-4))
                ((unboxed-gensym-6) (unsafe-flimag-part unboxed-gensym-4))
                ((unboxed-gensym-7) (unsafe-fl+ unboxed-gensym-2 unboxed-gensym-5))
                ((unboxed-gensym-8) (unsafe-fl+ unboxed-gensym-3 unboxed-gensym-6))
                ((unboxed-gensym-9) 3.0+6.0i)
                ((unboxed-gensym-10) (unsafe-flreal-part unboxed-gensym-9))
                ((unboxed-gensym-11) (unsafe-flimag-part unboxed-gensym-9)))
    (f unboxed-gensym-7 unboxed-gensym-10 unboxed-gensym-8 unboxed-gensym-11)))
(void)
