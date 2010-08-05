#lang typed/scheme
(require racket/unsafe/ops racket/flonum)
(let* ((unboxed-gensym-1 (->fl (expt 2 100)))
       (unboxed-gensym-2 1.0)
       (unboxed-gensym-3 2.0)
       (unboxed-gensym-4 (unsafe-fl+ unboxed-gensym-1 unboxed-gensym-2))
       (unboxed-gensym-5 unboxed-gensym-3))
  (unsafe-make-flrectangular unboxed-gensym-4 unboxed-gensym-5))
