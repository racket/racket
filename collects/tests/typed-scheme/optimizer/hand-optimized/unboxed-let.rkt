#lang racket

(require racket/unsafe/ops)

(let* ((unboxed-gensym-1 1.0+2.0i)
       (unboxed-gensym-2 (unsafe-flreal-part unboxed-gensym-1))
       (unboxed-gensym-3 (unsafe-flimag-part unboxed-gensym-1))
       (unboxed-gensym-4 2.0+4.0i)
       (unboxed-gensym-5 (unsafe-flreal-part unboxed-gensym-4))
       (unboxed-gensym-6 (unsafe-flimag-part unboxed-gensym-4))
       (unboxed-gensym-7 (unsafe-fl+ unboxed-gensym-2 unboxed-gensym-5)) ; t1-real
       (unboxed-gensym-8 (unsafe-fl+ unboxed-gensym-3 unboxed-gensym-6))) ; t1-imag
  (let* ((unboxed-gensym-1 3.0+6.0i)
         (unboxed-gensym-2 (unsafe-flreal-part unboxed-gensym-1))
         (unboxed-gensym-3 (unsafe-flimag-part unboxed-gensym-1))
         (unboxed-gensym-4 (unsafe-fl- unboxed-gensym-7 unboxed-gensym-2)) ; t2-real
         (unboxed-gensym-5 (unsafe-fl- unboxed-gensym-8 unboxed-gensym-3))) ; t2-imag
    (let* ((unboxed-gensym-1 4.0+8.0i)
           (unboxed-gensym-2 (unsafe-flreal-part unboxed-gensym-1)) ; t3-real
           (unboxed-gensym-3 (unsafe-flimag-part unboxed-gensym-1)) ; t3-imag
           (unboxed-gensym-4 (unsafe-fl+ unboxed-gensym-4 unboxed-gensym-2))
           (unboxed-gensym-5 (unsafe-fl+ unboxed-gensym-5 unboxed-gensym-3)))
      (unsafe-make-flrectangular unboxed-gensym-4 unboxed-gensym-5))))
(void)
