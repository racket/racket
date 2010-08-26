#lang typed/scheme
(require racket/unsafe/ops)
(map (lambda: ((x : Inexact-Complex))
              (string-append (real->decimal-string 
                              (let* ([unboxed-gensym-1 x]
                                     [unboxed-real-2 (unsafe-flreal-part unboxed-gensym-1)]
                                     [unboxed-imag-3 (unsafe-flimag-part unboxed-gensym-1)])
                                unboxed-real-2)
                              10)
                             (real->decimal-string 
                              (let* ([unboxed-gensym-1 x]
                                     [unboxed-real-2 (unsafe-flreal-part unboxed-gensym-1)]
                                     [unboxed-imag-3 (unsafe-flimag-part unboxed-gensym-1)])
                                unboxed-imag-3)
                              10)))
     (list
      (let* ((unboxed-float-1 1.0)
             (unboxed-real-2 2.0)
             (unboxed-imag-3 4.0)
             (unboxed-gensym-6 (unsafe-fl+ (unsafe-fl* unboxed-real-2
                                                       unboxed-real-2)
                                           (unsafe-fl* unboxed-imag-3
                                                       unboxed-imag-3)))
             (unboxed-real-4 (unsafe-fl/ (unsafe-fl* unboxed-float-1
                                                     unboxed-real-2)
                                         unboxed-gensym-6))
             (unboxed-imag-5 (unsafe-fl/ (unsafe-fl- 0.0
                                                     (unsafe-fl* unboxed-float-1
                                                                 unboxed-imag-3))
                                         unboxed-gensym-6)))
        (unsafe-make-flrectangular unboxed-real-4 unboxed-imag-5))

      (let* ((unboxed-real-1 1.0)
             (unboxed-imag-2 2.0)
             (unboxed-float-3 2.0)
             (unboxed-real-4 (unsafe-fl/ unboxed-real-1 unboxed-float-3))
             (unboxed-imag-5 (unsafe-fl/ unboxed-imag-2 unboxed-float-3)))
        (unsafe-make-flrectangular unboxed-real-4 unboxed-imag-5))

      (let* ((unboxed-float-1 1.0)
             (unboxed-real-2 2.0)
             (unboxed-imag-3 4.0)
             (unboxed-real-4 3.0)
             (unboxed-imag-5 6.0)
             (unboxed-gensym-10 (unsafe-fl+ (unsafe-fl* unboxed-real-2
                                                        unboxed-real-2)
                                            (unsafe-fl* unboxed-imag-3
                                                        unboxed-imag-3)))
             (unboxed-real-8 (unsafe-fl/ (unsafe-fl* unboxed-float-1
                                                     unboxed-real-2)
                                         unboxed-gensym-10))
             (unboxed-imag-9 (unsafe-fl/ (unsafe-fl- 0.0
                                                     (unsafe-fl* unboxed-float-1
                                                                 unboxed-imag-3))
                                         unboxed-gensym-10))
             (unboxed-gensym-11 (unsafe-fl+ (unsafe-fl* unboxed-real-4
                                                        unboxed-real-4)
                                            (unsafe-fl* unboxed-imag-5
                                                        unboxed-imag-5)))
             (unboxed-real-6 (unsafe-fl/ (unsafe-fl+ (unsafe-fl* unboxed-real-8
                                                                 unboxed-real-4)
                                                     (unsafe-fl* unboxed-imag-9
                                                                 unboxed-imag-5))
                                         unboxed-gensym-11))
             (unboxed-imag-7 (unsafe-fl/ (unsafe-fl- (unsafe-fl* unboxed-imag-9
                                                                 unboxed-real-4)
                                                     (unsafe-fl* unboxed-real-8
                                                                 unboxed-imag-5))
                                         unboxed-gensym-11)))
        (unsafe-make-flrectangular unboxed-real-6 unboxed-imag-7))

      (let* ((unboxed-real-1 1.0)
             (unboxed-imag-2 2.0)
             (unboxed-float-3 2.0)
             (unboxed-real-4 3.0)
             (unboxed-imag-5 6.0)
             (unboxed-real-8 (unsafe-fl/ unboxed-real-1 unboxed-float-3))
             (unboxed-imag-9 (unsafe-fl/ unboxed-imag-2 unboxed-float-3))
             (unboxed-gensym-11 (unsafe-fl+ (unsafe-fl* unboxed-real-4
                                                        unboxed-real-4)
                                            (unsafe-fl* unboxed-imag-5
                                                        unboxed-imag-5)))
             (unboxed-real-6 (unsafe-fl/ (unsafe-fl+ (unsafe-fl* unboxed-real-8
                                                                 unboxed-real-4)
                                                     (unsafe-fl* unboxed-imag-9
                                                                 unboxed-imag-5))
                                         unboxed-gensym-11))
             (unboxed-imag-7 (unsafe-fl/ (unsafe-fl- (unsafe-fl* unboxed-imag-9
                                                                 unboxed-real-4)
                                                     (unsafe-fl* unboxed-real-8
                                                                 unboxed-imag-5))
                                         unboxed-gensym-11)))
        (unsafe-make-flrectangular unboxed-real-6 unboxed-imag-7))

      (let* ((unboxed-real-1 1.0)
             (unboxed-imag-2 2.0)
             (unboxed-real-3 2.0)
             (unboxed-imag-4 4.0)
             (unboxed-float-5 3.0)
             (unboxed-gensym-10 (unsafe-fl+ (unsafe-fl* unboxed-real-3
                                                        unboxed-real-3)
                                            (unsafe-fl* unboxed-imag-4
                                                        unboxed-imag-4)))
             (unboxed-real-8 (unsafe-fl/ (unsafe-fl+ (unsafe-fl* unboxed-real-1
                                                                 unboxed-real-3)
                                                     (unsafe-fl* unboxed-imag-2
                                                                 unboxed-imag-4))
                                         unboxed-gensym-10))
             (unboxed-imag-9 (unsafe-fl/ (unsafe-fl- (unsafe-fl* unboxed-imag-2
                                                                 unboxed-real-3)
                                                     (unsafe-fl* unboxed-real-1
                                                                 unboxed-imag-4))
                                         unboxed-gensym-10))
             (unboxed-real-6 (unsafe-fl/ unboxed-real-8
                                         unboxed-float-5))
             (unboxed-imag-7 (unsafe-fl/ unboxed-imag-9
                                         unboxed-float-5)))
        (unsafe-make-flrectangular unboxed-real-6 unboxed-imag-7))
      (let* ((unboxed-real-1 1.0)
             (unboxed-imag-2 2.0)
             (unboxed-float-3 2.0)
             (unboxed-float-4 3.0)
             (unboxed-real-7 (unsafe-fl/ unboxed-real-1 unboxed-float-3))
             (unboxed-imag-8 (unsafe-fl/ unboxed-imag-2 unboxed-float-3))
             (unboxed-real-5 (unsafe-fl/ unboxed-real-7 unboxed-float-4))
             (unboxed-imag-6 (unsafe-fl/ unboxed-imag-8 unboxed-float-4)))
        (unsafe-make-flrectangular unboxed-real-5 unboxed-imag-6))
      (let* ((unboxed-float-1 1.0)
             (unboxed-float-2 2.0)
             (unboxed-real-3 3.0)
             (unboxed-imag-4 6.0)
             (unboxed-real-7 (unsafe-fl/ unboxed-float-1 unboxed-float-2))
             (unboxed-imag-8 0.0)
             (unboxed-gensym-10 (unsafe-fl+ (unsafe-fl* unboxed-real-3
                                                        unboxed-real-3)
                                            (unsafe-fl* unboxed-imag-4
                                                        unboxed-imag-4)))
             (unboxed-real-5 (unsafe-fl/ (unsafe-fl+ (unsafe-fl* unboxed-real-7
                                                                 unboxed-real-3)
                                                     (unsafe-fl* unboxed-imag-8
                                                                 unboxed-imag-4))
                                         unboxed-gensym-10))
             (unboxed-imag-6 (unsafe-fl/ (unsafe-fl- (unsafe-fl* unboxed-imag-8
                                                                 unboxed-real-3)
                                                     (unsafe-fl* unboxed-real-7
                                                                 unboxed-imag-4))
                                         unboxed-gensym-10)))
        (unsafe-make-flrectangular unboxed-real-5 unboxed-imag-6))))
