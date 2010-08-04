#lang typed/scheme #:optimize

(require racket/unsafe/ops)

(let* ((unboxed-gensym-1 1.0)
       (unboxed-gensym-2 2.0)
       (unboxed-gensym-3 2.0)
       (unboxed-gensym-4 4.0)
       (unboxed-gensym-5 (unsafe-fl+ unboxed-gensym-1 unboxed-gensym-3))
       (unboxed-gensym-6 (unsafe-fl+ unboxed-gensym-2 unboxed-gensym-4))
       (unboxed-gensym-7 3.0)
       (unboxed-gensym-8 6.0)
       (unboxed-gensym-9 4.0)
       (unboxed-gensym-10 8.0)
       (unboxed-gensym-11 (unsafe-fl+ unboxed-gensym-7 unboxed-gensym-9))
       (unboxed-gensym-12 (unsafe-fl+ unboxed-gensym-8 unboxed-gensym-10))
       (unboxed-gensym-13 (unsafe-fl+ unboxed-gensym-5 unboxed-gensym-11))
       (unboxed-gensym-14 (unsafe-fl+ unboxed-gensym-6 unboxed-gensym-12)))
  (unsafe-make-flrectangular unboxed-gensym-13 unboxed-gensym-14))
