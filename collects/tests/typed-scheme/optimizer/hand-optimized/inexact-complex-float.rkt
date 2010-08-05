#lang typed/scheme #:optimize
(require racket/unsafe/ops)
(let* ((unboxed-gensym-1 1.0)
       (unboxed-gensym-2 2.0)
       (unboxed-gensym-3 2.0)
       (unboxed-gensym-4 3.0)
       (unboxed-gensym-5 6.0)
       (unboxed-gensym-6 (unsafe-fl+ (unsafe-fl+ unboxed-gensym-1
                                                 unboxed-gensym-3)
                                     unboxed-gensym-4))
       (unboxed-gensym-7 (unsafe-fl+ unboxed-gensym-2
                                     unboxed-gensym-5)))
  (unsafe-make-flrectangular unboxed-gensym-6 unboxed-gensym-7))
(let* ((unboxed-gensym-1 1.0)
       (unboxed-gensym-2 2.0)
       (unboxed-gensym-3 4.0)
       (unboxed-gensym-4 3.0)
       (unboxed-gensym-5 6.0)
       (unboxed-gensym-6 (unsafe-fl- (unsafe-fl- unboxed-gensym-1
                                                 unboxed-gensym-2)
                                     unboxed-gensym-4))
       (unboxed-gensym-7 (unsafe-fl- (unsafe-fl- 0.0
                                                 unboxed-gensym-3)
                                     unboxed-gensym-5)))
  (unsafe-make-flrectangular unboxed-gensym-6 unboxed-gensym-7))
(let* ((unboxed-gensym-1 1.0)
       (unboxed-gensym-2 2.0)
       (unboxed-gensym-3 2.0)
       (unboxed-gensym-4 3.0)
       (unboxed-gensym-5 6.0)
       (unboxed-gensym-6 (unsafe-fl- (unsafe-fl- unboxed-gensym-1
                                                 unboxed-gensym-3)
                                     unboxed-gensym-4))
       (unboxed-gensym-7 (unsafe-fl- unboxed-gensym-2
                                     unboxed-gensym-5)))
  (unsafe-make-flrectangular unboxed-gensym-6 unboxed-gensym-7))
(let* ((unboxed-gensym-1 1.0)
       (unboxed-gensym-2 2.0)
       (unboxed-gensym-3 2.0)
       (unboxed-gensym-4 4.0)
       (unboxed-gensym-5 3.0)
       (unboxed-gensym-6 (unsafe-fl- (unsafe-fl- unboxed-gensym-1
                                                 unboxed-gensym-3)
                                     unboxed-gensym-5))
       (unboxed-gensym-7 (unsafe-fl- unboxed-gensym-2
                                     unboxed-gensym-4)))
  (unsafe-make-flrectangular unboxed-gensym-6 unboxed-gensym-7))
