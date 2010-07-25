#lang typed/scheme
(require racket/unsafe/ops racket/flonum)
(let* ((unboxed-gensym-1 (->fl (expt 2 100)))
       (unboxed-gensym-2 1.0+2.0i)
       (unboxed-gensym-3 (unsafe-flreal-part unboxed-gensym-2))
       (unboxed-gensym-4 (unsafe-flimag-part unboxed-gensym-2))
       (unboxed-gensym-5 (unsafe-fl+ unboxed-gensym-1 unboxed-gensym-3))
       (unboxed-gensym-6 unboxed-gensym-4))
  (unsafe-make-flrectangular unboxed-gensym-5 unboxed-gensym-6))
