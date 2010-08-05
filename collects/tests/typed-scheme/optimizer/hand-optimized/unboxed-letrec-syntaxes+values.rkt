#lang racket

(require racket/unsafe/ops)

(letrec-syntaxes+values
 (((s) (syntax-rules () [(_ x) x])))
 (((unboxed-real-1) 1.0)
  ((unboxed-imag-2) 2.0)
  ((unboxed-real-3) 2.0)
  ((unboxed-imag-4) 4.0)
  ((unboxed-real-5) (unsafe-fl+ unboxed-real-1 unboxed-real-3))
  ((unboxed-imag-6) (unsafe-fl+ unboxed-imag-2 unboxed-imag-4)))
 (let* ((unboxed-real-7 2.0)
        (unboxed-imag-8 4.0)
        (unboxed-real-9 (unsafe-fl+ unboxed-real-5 unboxed-real-7))
        (unboxed-imag-10 (unsafe-fl+ unboxed-imag-6 unboxed-imag-8)))
   (unsafe-make-flrectangular unboxed-real-9 unboxed-imag-10)))
(void)
