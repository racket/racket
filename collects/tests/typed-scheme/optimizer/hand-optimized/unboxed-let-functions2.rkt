#lang racket

(require racket/unsafe/ops)

;; function with multiple complex args
(let ((f (lambda (unboxed-real-1 unboxed-real-2 unboxed-imag-3 unboxed-imag-4)
           (let*-values (((unboxed-real-5) (unsafe-fl+ unboxed-real-1 unboxed-real-2))
                         ((unboxed-imag-6) (unsafe-fl+ unboxed-imag-3 unboxed-imag-4)))
             (unsafe-make-flrectangular unboxed-real-5 unboxed-imag-6)))))
  (let*-values (((unboxed-real-1) 1.0)
                ((unboxed-imag-2) 2.0)
                ((unboxed-real-3) 2.0)
                ((unboxed-imag-4) 4.0)
                ((unboxed-real-5) (unsafe-fl+ unboxed-real-1 unboxed-real-3))
                ((unboxed-imag-6) (unsafe-fl+ unboxed-imag-2 unboxed-imag-4))
                ((unboxed-real-7) 3.0)
                ((unboxed-imag-8) 6.0))
    (f unboxed-real-5 unboxed-real-7 unboxed-imag-6 unboxed-imag-8)))
(void)
