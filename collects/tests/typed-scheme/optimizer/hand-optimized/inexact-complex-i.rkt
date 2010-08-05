#lang typed/scheme
(require racket/unsafe/ops)
(let* ((unboxed-gensym-1 1.0)
       (unboxed-gensym-2 2.0)
       (unboxed-gensym-3 0.0)
       (unboxed-gensym-4 1.0)
       (unboxed-gensym-5 2.0)
       (unboxed-gensym-6 4.0)
       (unboxed-gensym-7 (unsafe-fl- (unsafe-fl* unboxed-gensym-3
                                                 unboxed-gensym-5)
                                     (unsafe-fl* unboxed-gensym-4
                                                 unboxed-gensym-6)))
       (unboxed-gensym-8 (unsafe-fl+ (unsafe-fl* unboxed-gensym-4
                                                 unboxed-gensym-5)
                                     (unsafe-fl* unboxed-gensym-3
                                                 unboxed-gensym-6)))
       (unboxed-gensym-9 (unsafe-fl+ unboxed-gensym-1 unboxed-gensym-7))
       (unboxed-gensym-10 (unsafe-fl+ unboxed-gensym-2 unboxed-gensym-8)))
  (unsafe-make-flrectangular unboxed-gensym-9 unboxed-gensym-10))
