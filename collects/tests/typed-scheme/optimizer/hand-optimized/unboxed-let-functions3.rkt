#lang racket

(require racket/unsafe/ops)

;; function with a mix of complex and non-complex args
(let ((f (lambda (unboxed-real-1 unboxed-imag-2 y)
           (let*-values (((unboxed-float-3) y)
                         ((unboxed-real-4) (unsafe-fl+ unboxed-real-1 unboxed-float-3))
                         ((unboxed-imag-5) unboxed-imag-2))
             (unsafe-make-flrectangular unboxed-real-4 unboxed-imag-5)))))
  (let*-values (((unboxed-real-1) 1.0)
                ((unboxed-imag-2) 2.0)
                ((unboxed-real-3) 2.0)
                ((unboxed-imag-4) 4.0)
                ((unboxed-real-5) (unsafe-fl+ unboxed-real-1 unboxed-real-3))
                ((unboxed-imag-6) (unsafe-fl+ unboxed-imag-2 unboxed-imag-4)))
    (f unboxed-real-5 unboxed-imag-6 3.0)))
(void)
