#lang typed/scheme
(require racket/unsafe/ops racket/flonum)
(let* ((unboxed-float-1 (->fl (expt 2 100)))
       (unboxed-real-2 1.0)
       (unboxed-imag-3 2.0)
       (unboxed-real-4 (unsafe-fl+ unboxed-float-1 unboxed-real-2))
       (unboxed-imag-5 unboxed-imag-3))
  (unsafe-make-flrectangular unboxed-real-4 unboxed-imag-5))
