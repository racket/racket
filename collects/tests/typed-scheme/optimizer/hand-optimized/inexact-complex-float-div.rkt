#lang typed/scheme
(require racket/unsafe/ops)
(map (lambda: ((x : Inexact-Complex))
              (string-append (real->decimal-string (unsafe-flreal-part x) 10)
                             (real->decimal-string (unsafe-flimag-part x) 10)))
     (list
      (let* ((unboxed-gensym-1 1.0)
             (unboxed-gensym-2 2.0+4.0i)
             (unboxed-gensym-3 (unsafe-flreal-part unboxed-gensym-2))
             (unboxed-gensym-4 (unsafe-flimag-part unboxed-gensym-2))
             (unboxed-gensym-7 (unsafe-fl+ (unsafe-fl* unboxed-gensym-3
                                                       unboxed-gensym-3)
                                           (unsafe-fl* unboxed-gensym-4
                                                       unboxed-gensym-4)))
             (unboxed-gensym-5 (unsafe-fl/ (unsafe-fl* unboxed-gensym-1
                                                       unboxed-gensym-3)
                                           unboxed-gensym-7))
             (unboxed-gensym-6 (unsafe-fl/ (unsafe-fl- 0.0
                                                       (unsafe-fl* unboxed-gensym-1
                                                                   unboxed-gensym-4))
                                           unboxed-gensym-7)))
        (unsafe-make-flrectangular unboxed-gensym-5 unboxed-gensym-6))

      (let* ((unboxed-gensym-1 1.0+2.0i)
             (unboxed-gensym-2 (unsafe-flreal-part unboxed-gensym-1))
             (unboxed-gensym-3 (unsafe-flimag-part unboxed-gensym-1))
             (unboxed-gensym-4 2.0)
             (unboxed-gensym-5 (unsafe-fl/ unboxed-gensym-2 unboxed-gensym-4))
             (unboxed-gensym-6 (unsafe-fl/ unboxed-gensym-3 unboxed-gensym-4)))
        (unsafe-make-flrectangular unboxed-gensym-5 unboxed-gensym-6))

      (let* ((unboxed-gensym-1 1.0)
             (unboxed-gensym-2 2.0+4.0i)
             (unboxed-gensym-3 (unsafe-flreal-part unboxed-gensym-2))
             (unboxed-gensym-4 (unsafe-flimag-part unboxed-gensym-2))
             (unboxed-gensym-5 3.0+6.0i)
             (unboxed-gensym-6 (unsafe-flreal-part unboxed-gensym-5))
             (unboxed-gensym-7 (unsafe-flimag-part unboxed-gensym-5))
             (unboxed-gensym-12 (unsafe-fl+ (unsafe-fl* unboxed-gensym-3
                                                        unboxed-gensym-3)
                                            (unsafe-fl* unboxed-gensym-4
                                                        unboxed-gensym-4)))
             (unboxed-gensym-10 (unsafe-fl/ (unsafe-fl* unboxed-gensym-1
                                                        unboxed-gensym-3)
                                            unboxed-gensym-12))
             (unboxed-gensym-11 (unsafe-fl/ (unsafe-fl- 0.0
                                                        (unsafe-fl* unboxed-gensym-1
                                                                    unboxed-gensym-4))
                                            unboxed-gensym-12))
             (unboxed-gensym-13 (unsafe-fl+ (unsafe-fl* unboxed-gensym-6
                                                        unboxed-gensym-6)
                                            (unsafe-fl* unboxed-gensym-7
                                                        unboxed-gensym-7)))
             (unboxed-gensym-8 (unsafe-fl/ (unsafe-fl+ (unsafe-fl* unboxed-gensym-10
                                                                   unboxed-gensym-6)
                                                       (unsafe-fl* unboxed-gensym-11
                                                                   unboxed-gensym-7))
                                           unboxed-gensym-13))
             (unboxed-gensym-9 (unsafe-fl/ (unsafe-fl- (unsafe-fl* unboxed-gensym-11
                                                                   unboxed-gensym-6)
                                                       (unsafe-fl* unboxed-gensym-10
                                                                   unboxed-gensym-7))
                                           unboxed-gensym-13)))
        (unsafe-make-flrectangular unboxed-gensym-8 unboxed-gensym-9))

      (let* ((unboxed-gensym-1 1.0+2.0i)
             (unboxed-gensym-2 (unsafe-flreal-part unboxed-gensym-1))
             (unboxed-gensym-3 (unsafe-flimag-part unboxed-gensym-1))
             (unboxed-gensym-4 2.0)
             (unboxed-gensym-5 3.0+6.0i)
             (unboxed-gensym-6 (unsafe-flreal-part unboxed-gensym-5))
             (unboxed-gensym-7 (unsafe-flimag-part unboxed-gensym-5))
             (unboxed-gensym-10 (unsafe-fl/ unboxed-gensym-2 unboxed-gensym-4))
             (unboxed-gensym-11 (unsafe-fl/ unboxed-gensym-3 unboxed-gensym-4))
             (unboxed-gensym-13 (unsafe-fl+ (unsafe-fl* unboxed-gensym-6
                                                        unboxed-gensym-6)
                                            (unsafe-fl* unboxed-gensym-7
                                                        unboxed-gensym-7)))
             (unboxed-gensym-8 (unsafe-fl/ (unsafe-fl+ (unsafe-fl* unboxed-gensym-10
                                                                   unboxed-gensym-6)
                                                       (unsafe-fl* unboxed-gensym-11
                                                                   unboxed-gensym-7))
                                           unboxed-gensym-13))
             (unboxed-gensym-9 (unsafe-fl/ (unsafe-fl- (unsafe-fl* unboxed-gensym-11
                                                                   unboxed-gensym-6)
                                                       (unsafe-fl* unboxed-gensym-10
                                                                   unboxed-gensym-7))
                                           unboxed-gensym-13)))
        (unsafe-make-flrectangular unboxed-gensym-8 unboxed-gensym-9))

      (let* ((unboxed-gensym-1 1.0+2.0i)
             (unboxed-gensym-2 (unsafe-flreal-part unboxed-gensym-1))
             (unboxed-gensym-3 (unsafe-flimag-part unboxed-gensym-1))
             (unboxed-gensym-4 2.0+4.0i)
             (unboxed-gensym-5 (unsafe-flreal-part unboxed-gensym-4))
             (unboxed-gensym-6 (unsafe-flimag-part unboxed-gensym-4))
             (unboxed-gensym-7 3.0)
             (unboxed-gensym-12 (unsafe-fl+ (unsafe-fl* unboxed-gensym-5
                                                        unboxed-gensym-5)
                                            (unsafe-fl* unboxed-gensym-6
                                                        unboxed-gensym-6)))
             (unboxed-gensym-10 (unsafe-fl/ (unsafe-fl+ (unsafe-fl* unboxed-gensym-2
                                                                    unboxed-gensym-5)
                                                        (unsafe-fl* unboxed-gensym-3
                                                                    unboxed-gensym-6))
                                            unboxed-gensym-12))
             (unboxed-gensym-11 (unsafe-fl/ (unsafe-fl- (unsafe-fl* unboxed-gensym-3
                                                                    unboxed-gensym-5)
                                                        (unsafe-fl* unboxed-gensym-2
                                                                    unboxed-gensym-6))
                                            unboxed-gensym-12))
             (unboxed-gensym-8 (unsafe-fl/ unboxed-gensym-10
                                           unboxed-gensym-7))
             (unboxed-gensym-9 (unsafe-fl/ unboxed-gensym-11
                                           unboxed-gensym-7)))
        (unsafe-make-flrectangular unboxed-gensym-8 unboxed-gensym-9))
      (let* ((unboxed-gensym-1 1.0+2.0i)
             (unboxed-gensym-2 (unsafe-flreal-part unboxed-gensym-1))
             (unboxed-gensym-3 (unsafe-flimag-part unboxed-gensym-1))
             (unboxed-gensym-4 2.0)
             (unboxed-gensym-5 3.0)
             (unboxed-gensym-8 (unsafe-fl/ unboxed-gensym-2 unboxed-gensym-4))
             (unboxed-gensym-9 (unsafe-fl/ unboxed-gensym-3 unboxed-gensym-4))
             (unboxed-gensym-6 (unsafe-fl/ unboxed-gensym-8 unboxed-gensym-5))
             (unboxed-gensym-7 (unsafe-fl/ unboxed-gensym-9 unboxed-gensym-5)))
        (unsafe-make-flrectangular unboxed-gensym-6 unboxed-gensym-7))
      (let* ((unboxed-gensym-1 1.0)
             (unboxed-gensym-2 2.0)
             (unboxed-gensym-3 3.0+6.0i)
             (unboxed-gensym-4 (unsafe-flreal-part unboxed-gensym-3))
             (unboxed-gensym-5 (unsafe-flimag-part unboxed-gensym-3))
             (unboxed-gensym-8 (unsafe-fl/ unboxed-gensym-1 unboxed-gensym-2))
             (unboxed-gensym-9 0.0)
             (unboxed-gensym-11 (unsafe-fl+ (unsafe-fl* unboxed-gensym-4
                                                        unboxed-gensym-4)
                                            (unsafe-fl* unboxed-gensym-5
                                                        unboxed-gensym-5)))
             (unboxed-gensym-6 (unsafe-fl/ (unsafe-fl+ (unsafe-fl* unboxed-gensym-8
                                                                   unboxed-gensym-4)
                                                       (unsafe-fl* unboxed-gensym-9
                                                                   unboxed-gensym-5))
                                           unboxed-gensym-11))
             (unboxed-gensym-7 (unsafe-fl/ (unsafe-fl- (unsafe-fl* unboxed-gensym-9
                                                                   unboxed-gensym-4)
                                                       (unsafe-fl* unboxed-gensym-8
                                                                   unboxed-gensym-5))
                                           unboxed-gensym-11)))
        (unsafe-make-flrectangular unboxed-gensym-6 unboxed-gensym-7))))
