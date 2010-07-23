#lang racket

(require racket/unsafe/ops)

(let* ((unboxed-gensym-1 1.0+2.0i)
       (unboxed-gensym-2 (unsafe-flreal-part unboxed-gensym-1))
       (unboxed-gensym-3 (unsafe-flimag-part unboxed-gensym-1))
       (unboxed-gensym-4 2.0+4.0i)
       (unboxed-gensym-5 (unsafe-flreal-part unboxed-gensym-4))
       (unboxed-gensym-6 (unsafe-flimag-part unboxed-gensym-4))
       (unboxed-gensym-7 (unsafe-fl+ unboxed-gensym-2 unboxed-gensym-5)) ; t1-real
       (unboxed-gensym-8 (unsafe-fl+ unboxed-gensym-3 unboxed-gensym-6)) ; t1-imag
       (unboxed-gensym-9 3.0+6.0i)
       (unboxed-gensym-10 (unsafe-flreal-part unboxed-gensym-9))
       (unboxed-gensym-11 (unsafe-flimag-part unboxed-gensym-9))
       (unboxed-gensym-12 (unsafe-fl- unboxed-gensym-7 unboxed-gensym-10)) ; t2-real
       (unboxed-gensym-13 (unsafe-fl- unboxed-gensym-8 unboxed-gensym-11)) ; t2-imag
       (unboxed-gensym-14 4.0+8.0i)
       (unboxed-gensym-15 (unsafe-flreal-part unboxed-gensym-14))
       (unboxed-gensym-16 (unsafe-flimag-part unboxed-gensym-14))
       (unboxed-gensym-17 (unsafe-fl- unboxed-gensym-7 unboxed-gensym-15)) ; t3-real
       (unboxed-gensym-18 (unsafe-fl- unboxed-gensym-8 unboxed-gensym-16)) ; t3-imag
       (unboxed-gensym-19 (unsafe-fl+ unboxed-gensym-12 unboxed-gensym-17))
       (unboxed-gensym-20 (unsafe-fl+ unboxed-gensym-13 unboxed-gensym-18)))
  (unsafe-make-flrectangular unboxed-gensym-19 unboxed-gensym-20))
(void)
