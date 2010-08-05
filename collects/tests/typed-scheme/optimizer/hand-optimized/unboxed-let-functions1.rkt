#lang racket

(require racket/unsafe/ops)

;; simple case, function with single complex arg
(let ((f (lambda (unboxed-real-1 unboxed-imag-2)
           (let*-values (((unboxed-real-3) 3.0)
                         ((unboxed-imag-4) 6.0)
                         ((unboxed-real-5) (unsafe-fl+ unboxed-real-1 unboxed-real-3))
                         ((unboxed-imag-6) (unsafe-fl+ unboxed-imag-2 unboxed-imag-4)))
             (unsafe-make-flrectangular unboxed-real-5 unboxed-imag-6)))))
  (let*-values (((unboxed-real-1) 1.0)
                ((unboxed-imag-2) 2.0)
                ((unboxed-real-3) 2.0)
                ((unboxed-imag-4) 4.0)
                ((unboxed-real-5) (unsafe-fl+ unboxed-real-1 unboxed-real-3))
                ((unboxed-imag-6) (unsafe-fl+ unboxed-imag-2 unboxed-imag-4)))
    (f unboxed-real-5 unboxed-imag-6)))
(void)
