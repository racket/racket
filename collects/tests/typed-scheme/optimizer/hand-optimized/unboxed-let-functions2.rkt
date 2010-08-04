#lang racket

(require racket/unsafe/ops)

;; function with multiple complex args
(let ((f (lambda (unboxed-real-1 unboxed-real-2 unboxed-imag-3 unboxed-imag-4)
           (let*-values (((unboxed-gensym-5) (unsafe-fl+ unboxed-real-1 unboxed-real-2))
                         ((unboxed-gensym-6) (unsafe-fl+ unboxed-imag-3 unboxed-imag-4)))
             (unsafe-make-flrectangular unboxed-gensym-5 unboxed-gensym-6)))))
  (let*-values (((unboxed-gensym-1) 1.0)
                ((unboxed-gensym-2) 2.0)
                ((unboxed-gensym-3) 2.0)
                ((unboxed-gensym-4) 4.0)
                ((unboxed-gensym-5) (unsafe-fl+ unboxed-gensym-1 unboxed-gensym-3))
                ((unboxed-gensym-6) (unsafe-fl+ unboxed-gensym-2 unboxed-gensym-4))
                ((unboxed-gensym-7) 3.0)
                ((unboxed-gensym-8) 6.0))
    (f unboxed-gensym-5 unboxed-gensym-7 unboxed-gensym-6 unboxed-gensym-8)))
(void)
