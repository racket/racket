#lang racket

(require racket/unsafe/ops)

;; simple case, function with single complex arg
(let ((f (lambda (unboxed-real-1 unboxed-imag-2)
           (let*-values (((unboxed-gensym-3) 3.0+6.0i)
                         ((unboxed-gensym-4) (unsafe-flreal-part unboxed-gensym-3))
                         ((unboxed-gensym-5) (unsafe-flimag-part unboxed-gensym-3))
                         ((unboxed-gensym-6) (unsafe-fl+ unboxed-real-1 unboxed-gensym-4))
                         ((unboxed-gensym-7) (unsafe-fl+ unboxed-imag-2 unboxed-gensym-5)))
             (unsafe-make-flrectangular unboxed-gensym-6 unboxed-gensym-7)))))
  (let*-values (((unboxed-gensym-1) 1.0+2.0i)
                ((unboxed-gensym-2) (unsafe-flreal-part unboxed-gensym-1))
                ((unboxed-gensym-3) (unsafe-flimag-part unboxed-gensym-1))
                ((unboxed-gensym-4) 2.0+4.0i)
                ((unboxed-gensym-5) (unsafe-flreal-part unboxed-gensym-4))
                ((unboxed-gensym-6) (unsafe-flimag-part unboxed-gensym-4))
                ((unboxed-gensym-7) (unsafe-fl+ unboxed-gensym-2 unboxed-gensym-5))
                ((unboxed-gensym-8) (unsafe-fl+ unboxed-gensym-3 unboxed-gensym-6)))
    (f unboxed-gensym-7 unboxed-gensym-8)))
(void)
