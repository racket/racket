#lang typed/scheme
(require racket/unsafe/ops)
(let* ((unboxed-float-1 1.0)
       (unboxed-real-2 2.0)
       (unboxed-imag-3 4.0)
       (unboxed-real-4 (unsafe-fl* unboxed-float-1
                                   unboxed-real-2))
       (unboxed-imag-5 (unsafe-fl* unboxed-float-1
                                   unboxed-imag-3)))
  (unsafe-make-flrectangular unboxed-real-4 unboxed-imag-5))
(let* ((unboxed-real-1 1.0)
       (unboxed-imag-2 2.0)
       (unboxed-float-3 2.0)
       (unboxed-real-4 (unsafe-fl* unboxed-real-1
                                   unboxed-float-3))
       (unboxed-imag-5 (unsafe-fl* unboxed-imag-2
                                   unboxed-float-3)))
  (unsafe-make-flrectangular unboxed-real-4 unboxed-imag-5))
(let* ((unboxed-float-1 1.0)
       (unboxed-real-2 2.0)
       (unboxed-imag-3 4.0)
       (unboxed-real-4 3.0)
       (unboxed-imag-5 6.0)
       (unboxed-real-8 (unsafe-fl* unboxed-float-1
                                   unboxed-real-2))
       (unboxed-imag-9 (unsafe-fl* unboxed-float-1
                                   unboxed-imag-3))
       (unboxed-real-6 (unsafe-fl- (unsafe-fl* unboxed-real-8
                                               unboxed-real-4)
                                   (unsafe-fl* unboxed-imag-9
                                               unboxed-imag-5)))
       (unboxed-imag-7 (unsafe-fl+ (unsafe-fl* unboxed-imag-9
                                               unboxed-real-4)
                                   (unsafe-fl* unboxed-real-8
                                               unboxed-imag-5))))
  (unsafe-make-flrectangular unboxed-real-6 unboxed-imag-7))
(let* ((unboxed-real-1 1.0)
       (unboxed-imag-2 2.0)
       (unboxed-float-3 2.0)
       (unboxed-real-4 3.0)
       (unboxed-imag-5 6.0)
       (unboxed-real-8 (unsafe-fl* unboxed-real-1
                                   unboxed-float-3))
       (unboxed-imag-9 (unsafe-fl* unboxed-imag-2
                                   unboxed-float-3))
       (unboxed-real-6 (unsafe-fl- (unsafe-fl* unboxed-real-8
                                               unboxed-real-4)
                                   (unsafe-fl* unboxed-imag-9
                                               unboxed-imag-5)))
       (unboxed-imag-7 (unsafe-fl+ (unsafe-fl* unboxed-imag-9
                                               unboxed-real-4)
                                   (unsafe-fl* unboxed-real-8
                                               unboxed-imag-5))))
  (unsafe-make-flrectangular unboxed-real-6 unboxed-imag-7))
(let* ((unboxed-real-1 1.0)
       (unboxed-imag-2 2.0)
       (unboxed-real-3 2.0)
       (unboxed-imag-4 4.0)
       (unboxed-float-5 3.0)
       (unboxed-real-8 (unsafe-fl- (unsafe-fl* unboxed-real-1
                                               unboxed-real-3)
                                   (unsafe-fl* unboxed-imag-2
                                               unboxed-imag-4)))
       (unboxed-imag-9 (unsafe-fl+ (unsafe-fl* unboxed-imag-2
                                               unboxed-real-3)
                                   (unsafe-fl* unboxed-real-1
                                               unboxed-imag-4)))
       (unboxed-real-6 (unsafe-fl* unboxed-real-8
                                   unboxed-float-5))
       (unboxed-imag-7 (unsafe-fl* unboxed-imag-9
                                   unboxed-float-5)))
  (unsafe-make-flrectangular unboxed-real-6 unboxed-imag-7))
(let* ((unboxed-real-1 1.0)
       (unboxed-imag-2 2.0)
       (unboxed-float-3 2.0)
       (unboxed-float-4 3.0)
       (unboxed-real-7 (unsafe-fl* unboxed-real-1
                                   unboxed-float-3))
       (unboxed-imag-8 (unsafe-fl* unboxed-imag-2
                                   unboxed-float-3))
       (unboxed-real-5 (unsafe-fl* unboxed-real-7
                                   unboxed-float-4))
       (unboxed-imag-6 (unsafe-fl* unboxed-imag-8
                                   unboxed-float-4)))
  (unsafe-make-flrectangular unboxed-real-5 unboxed-imag-6))
