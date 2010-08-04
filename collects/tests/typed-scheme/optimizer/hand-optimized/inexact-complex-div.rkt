(module inexact-complex-div typed/scheme
  (require racket/unsafe/ops)
  (let* ((unboxed-gensym-1 1.0)
         (unboxed-gensym-2 2.0)
         (unboxed-gensym-3 2.0)
         (unboxed-gensym-4 4.0)
         (unboxed-gensym-5 3.0)
         (unboxed-gensym-6 6.0)
         (unboxed-gensym-11 (unsafe-fl+ (unsafe-fl* unboxed-gensym-3 unboxed-gensym-3)
                                        (unsafe-fl* unboxed-gensym-4 unboxed-gensym-4)))
         (unboxed-gensym-9 (unsafe-fl/ (unsafe-fl+ (unsafe-fl* unboxed-gensym-1
                                                               unboxed-gensym-3)
                                                   (unsafe-fl* unboxed-gensym-2
                                                               unboxed-gensym-4))
                                       unboxed-gensym-11))
         (unboxed-gensym-10 (unsafe-fl/ (unsafe-fl- (unsafe-fl* unboxed-gensym-2
                                                                unboxed-gensym-3)
                                                    (unsafe-fl* unboxed-gensym-1
                                                                unboxed-gensym-4))
                                        unboxed-gensym-11))
         (unboxed-gensym-12 (unsafe-fl+ (unsafe-fl* unboxed-gensym-5 unboxed-gensym-5)
                                        (unsafe-fl* unboxed-gensym-6 unboxed-gensym-6)))
         (unboxed-gensym-7 (unsafe-fl/ (unsafe-fl+ (unsafe-fl* unboxed-gensym-9
                                                               unboxed-gensym-5)
                                                   (unsafe-fl* unboxed-gensym-10
                                                               unboxed-gensym-6))
                                       unboxed-gensym-12))
         (unboxed-gensym-8 (unsafe-fl/ (unsafe-fl- (unsafe-fl* unboxed-gensym-10
                                                               unboxed-gensym-5)
                                                   (unsafe-fl* unboxed-gensym-9
                                                               unboxed-gensym-6))
                                       unboxed-gensym-12)))
    (unsafe-make-flrectangular unboxed-gensym-7 unboxed-gensym-8)))
