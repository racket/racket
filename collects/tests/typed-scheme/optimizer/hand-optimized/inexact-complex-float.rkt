#lang typed/scheme #:optimize
(require racket/unsafe/ops)
(let* ((unboxed-real-1 1.0)
       (unboxed-imag-2 2.0)
       (unboxed-float-3 2.0)
       (unboxed-real-4 3.0)
       (unboxed-imag-5 6.0)
       (unboxed-real-6 (unsafe-fl+ (unsafe-fl+ unboxed-real-1
                                               unboxed-float-3)
                                   unboxed-real-4))
       (unboxed-imag-7 (unsafe-fl+ unboxed-imag-2
                                   unboxed-imag-5)))
  (unsafe-make-flrectangular unboxed-real-6 unboxed-imag-7))
(let* ((unboxed-float-1 1.0)
       (unboxed-real-2 2.0)
       (unboxed-imag-3 4.0)
       (unboxed-real-4 3.0)
       (unboxed-imag-5 6.0)
       (unboxed-real-6 (unsafe-fl- (unsafe-fl- unboxed-float-1
                                               unboxed-real-2)
                                   unboxed-real-4))
       (unboxed-imag-7 (unsafe-fl- (unsafe-fl- 0.0
                                               unboxed-imag-3)
                                   unboxed-imag-5)))
  (unsafe-make-flrectangular unboxed-real-6 unboxed-imag-7))
(let* ((unboxed-real-1 1.0)
       (unboxed-imag-2 2.0)
       (unboxed-float-3 2.0)
       (unboxed-real-4 3.0)
       (unboxed-imag-5 6.0)
       (unboxed-real-6 (unsafe-fl- (unsafe-fl- unboxed-real-1
                                               unboxed-float-3)
                                   unboxed-real-4))
       (unboxed-imag-7 (unsafe-fl- unboxed-imag-2
                                   unboxed-imag-5)))
  (unsafe-make-flrectangular unboxed-real-6 unboxed-imag-7))
(let* ((unboxed-real-1 1.0)
       (unboxed-imag-2 2.0)
       (unboxed-real-3 2.0)
       (unboxed-imag-4 4.0)
       (unboxed-float-5 3.0)
       (unboxed-real-6 (unsafe-fl- (unsafe-fl- unboxed-real-1
                                               unboxed-real-3)
                                   unboxed-float-5))
       (unboxed-imag-7 (unsafe-fl- unboxed-imag-2
                                   unboxed-imag-4)))
  (unsafe-make-flrectangular unboxed-real-6 unboxed-imag-7))
