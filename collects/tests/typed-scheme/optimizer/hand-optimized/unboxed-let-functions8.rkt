#lang racket

(require racket/unsafe/ops)

(letrec-values
 (((f) (lambda (x)
         (let*-values (((unboxed-gensym-1) x)
                       ((unboxed-real-2) (unsafe-flreal-part unboxed-gensym-1))
                       ((unboxed-imag-3) (unsafe-flimag-part unboxed-gensym-1))
                       ((unboxed-real-4) 2.0)
                       ((unboxed-imag-5) 4.0)
                       ((unboxed-real-6) (unsafe-fl+ unboxed-real-2 unboxed-real-4))
                       ((unboxed-imag-7) (unsafe-fl+ unboxed-imag-3 unboxed-imag-5)))
           (unsafe-make-flrectangular unboxed-real-6 unboxed-imag-7))))
  ((g) f))
 (f 1.0+2.0i))
(void)
