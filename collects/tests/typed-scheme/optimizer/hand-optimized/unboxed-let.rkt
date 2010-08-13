#lang racket

(require racket/unsafe/ops)

(let* ((unboxed-real-1 1.0)
       (unboxed-imag-2 2.0)
       (unboxed-real-3 2.0)
       (unboxed-imag-4 4.0)
       (unboxed-real-5 (unsafe-fl+ unboxed-real-1 unboxed-real-3)) ; t1-real
       (unboxed-imag-6 (unsafe-fl+ unboxed-imag-2 unboxed-imag-4))) ; t1-imag
  (let* ((unboxed-real-1 3.0)
         (unboxed-imag-2 6.0)
         (unboxed-real-3 (unsafe-fl- unboxed-real-5 unboxed-real-1)) ; t2-real
         (unboxed-imag-4 (unsafe-fl- unboxed-imag-6 unboxed-imag-2))) ; t2-imag
    (let* ((unboxed-real-1 4.0) ; t3-real
           (unboxed-imag-2 8.0) ; t3-imag
           (unboxed-real-3 (unsafe-fl+ unboxed-real-3 unboxed-real-1))
           (unboxed-imag-4 (unsafe-fl+ unboxed-imag-4 unboxed-imag-2)))
      (unsafe-make-flrectangular unboxed-real-3 unboxed-imag-4))))
(void)
