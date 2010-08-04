#lang racket

(require racket/unsafe/ops)

(let* ((unboxed-gensym-1 1.0)
       (unboxed-gensym-2 2.0)
       (unboxed-gensym-3 2.0)
       (unboxed-gensym-4 4.0)
       (unboxed-gensym-5 (unsafe-fl+ unboxed-gensym-1 unboxed-gensym-3)) ; t1-real
       (unboxed-gensym-6 (unsafe-fl+ unboxed-gensym-2 unboxed-gensym-4))) ; t1-imag
  (let* ((unboxed-gensym-1 3.0)
         (unboxed-gensym-2 6.0)
         (unboxed-gensym-3 (unsafe-fl- unboxed-gensym-5 unboxed-gensym-1)) ; t2-real
         (unboxed-gensym-4 (unsafe-fl- unboxed-gensym-6 unboxed-gensym-2))) ; t2-imag
    (let* ((unboxed-gensym-1 4.0) ; t3-real
           (unboxed-gensym-2 8.0) ; t3-imag
           (unboxed-gensym-3 (unsafe-fl+ unboxed-gensym-3 unboxed-gensym-1))
           (unboxed-gensym-4 (unsafe-fl+ unboxed-gensym-4 unboxed-gensym-2)))
      (unsafe-make-flrectangular unboxed-gensym-3 unboxed-gensym-4))))
(void)
