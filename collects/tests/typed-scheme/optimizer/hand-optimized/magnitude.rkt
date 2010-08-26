#lang racket/base

(require racket/unsafe/ops)

(let* ((unboxed-real-1 '3.0)
       (unboxed-imag-2 '4.0)
       (unboxed-real-3
        (unsafe-flsqrt
         (unsafe-fl+ (unsafe-fl* unboxed-real-1 unboxed-real-1) (unsafe-fl* unboxed-imag-2 unboxed-imag-2)))))
  unboxed-real-3)

(void)
