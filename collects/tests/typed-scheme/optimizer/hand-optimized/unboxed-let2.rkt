#lang typed/scheme #:optimize

(require racket/unsafe/ops)

(let* ((unboxed-real-1 1.0)
       (unboxed-imag-2 2.0)
       (unboxed-real-3 2.0)
       (unboxed-imag-4 4.0)
       (unboxed-real-5 (unsafe-fl+ unboxed-real-1 unboxed-real-3))
       (unboxed-imag-6 (unsafe-fl+ unboxed-imag-2 unboxed-imag-4))
       (unboxed-real-7 3.0)
       (unboxed-imag-8 6.0)
       (unboxed-real-9 4.0)
       (unboxed-imag-10 8.0)
       (unboxed-real-11 (unsafe-fl+ unboxed-real-7 unboxed-real-9))
       (unboxed-imag-12 (unsafe-fl+ unboxed-imag-8 unboxed-imag-10))
       (unboxed-real-13 (unsafe-fl+ unboxed-real-5 unboxed-real-11))
       (unboxed-imag-14 (unsafe-fl+ unboxed-imag-6 unboxed-imag-12)))
  (unsafe-make-flrectangular unboxed-real-13 unboxed-imag-14))
