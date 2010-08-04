#lang typed/scheme
(require racket/unsafe/ops)
(map (lambda: ((x : Inexact-Complex))
              (string-append (real->decimal-string (unsafe-flreal-part x) 10)
                             (real->decimal-string (unsafe-flimag-part x) 10)))
     (list
      (let* ((unboxed-gensym-1 1.0)
             (unboxed-gensym-2 2.0)
             (unboxed-gensym-3 4.0)
             (unboxed-gensym-6 (unsafe-fl+ (unsafe-fl* unboxed-gensym-2
                                                       unboxed-gensym-2)
                                           (unsafe-fl* unboxed-gensym-3
                                                       unboxed-gensym-3)))
             (unboxed-gensym-4 (unsafe-fl/ (unsafe-fl* unboxed-gensym-1
                                                       unboxed-gensym-2)
                                           unboxed-gensym-6))
             (unboxed-gensym-5 (unsafe-fl/ (unsafe-fl- 0.0
                                                       (unsafe-fl* unboxed-gensym-1
                                                                   unboxed-gensym-3))
                                           unboxed-gensym-6)))
        (unsafe-make-flrectangular unboxed-gensym-4 unboxed-gensym-5))

      (let* ((unboxed-gensym-1 1.0)
             (unboxed-gensym-2 2.0)
             (unboxed-gensym-3 2.0)
             (unboxed-gensym-4 (unsafe-fl/ unboxed-gensym-1 unboxed-gensym-3))
             (unboxed-gensym-5 (unsafe-fl/ unboxed-gensym-2 unboxed-gensym-3)))
        (unsafe-make-flrectangular unboxed-gensym-4 unboxed-gensym-5))

      (let* ((unboxed-gensym-1 1.0)
             (unboxed-gensym-2 2.0)
             (unboxed-gensym-3 4.0)
             (unboxed-gensym-4 3.0)
             (unboxed-gensym-5 6.0)
             (unboxed-gensym-10 (unsafe-fl+ (unsafe-fl* unboxed-gensym-2
                                                        unboxed-gensym-2)
                                            (unsafe-fl* unboxed-gensym-3
                                                        unboxed-gensym-3)))
             (unboxed-gensym-8 (unsafe-fl/ (unsafe-fl* unboxed-gensym-1
                                                       unboxed-gensym-2)
                                            unboxed-gensym-10))
             (unboxed-gensym-9 (unsafe-fl/ (unsafe-fl- 0.0
                                                       (unsafe-fl* unboxed-gensym-1
                                                                   unboxed-gensym-3))
                                           unboxed-gensym-10))
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
        (unsafe-make-flrectangular unboxed-gensym-6 unboxed-gensym-7))

      (let* ((unboxed-gensym-1 1.0)
             (unboxed-gensym-2 2.0)
             (unboxed-gensym-3 2.0)
             (unboxed-gensym-4 3.0)
             (unboxed-gensym-5 6.0)
             (unboxed-gensym-8 (unsafe-fl/ unboxed-gensym-1 unboxed-gensym-3))
             (unboxed-gensym-9 (unsafe-fl/ unboxed-gensym-2 unboxed-gensym-3))
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
        (unsafe-make-flrectangular unboxed-gensym-6 unboxed-gensym-7))

      (let* ((unboxed-gensym-1 1.0)
             (unboxed-gensym-2 2.0)
             (unboxed-gensym-3 2.0)
             (unboxed-gensym-4 4.0)
             (unboxed-gensym-5 3.0)
             (unboxed-gensym-10 (unsafe-fl+ (unsafe-fl* unboxed-gensym-3
                                                        unboxed-gensym-3)
                                            (unsafe-fl* unboxed-gensym-4
                                                        unboxed-gensym-4)))
             (unboxed-gensym-8 (unsafe-fl/ (unsafe-fl+ (unsafe-fl* unboxed-gensym-1
                                                                   unboxed-gensym-3)
                                                       (unsafe-fl* unboxed-gensym-2
                                                                   unboxed-gensym-4))
                                            unboxed-gensym-10))
             (unboxed-gensym-9 (unsafe-fl/ (unsafe-fl- (unsafe-fl* unboxed-gensym-2
                                                                   unboxed-gensym-3)
                                                       (unsafe-fl* unboxed-gensym-1
                                                                   unboxed-gensym-4))
                                            unboxed-gensym-10))
             (unboxed-gensym-6 (unsafe-fl/ unboxed-gensym-8
                                           unboxed-gensym-5))
             (unboxed-gensym-7 (unsafe-fl/ unboxed-gensym-9
                                           unboxed-gensym-5)))
        (unsafe-make-flrectangular unboxed-gensym-6 unboxed-gensym-7))
      (let* ((unboxed-gensym-1 1.0)
             (unboxed-gensym-2 2.0)
             (unboxed-gensym-3 2.0)
             (unboxed-gensym-4 3.0)
             (unboxed-gensym-7 (unsafe-fl/ unboxed-gensym-1 unboxed-gensym-3))
             (unboxed-gensym-8 (unsafe-fl/ unboxed-gensym-2 unboxed-gensym-3))
             (unboxed-gensym-5 (unsafe-fl/ unboxed-gensym-7 unboxed-gensym-4))
             (unboxed-gensym-6 (unsafe-fl/ unboxed-gensym-8 unboxed-gensym-4)))
        (unsafe-make-flrectangular unboxed-gensym-5 unboxed-gensym-6))
      (let* ((unboxed-gensym-1 1.0)
             (unboxed-gensym-2 2.0)
             (unboxed-gensym-3 3.0)
             (unboxed-gensym-4 6.0)
             (unboxed-gensym-7 (unsafe-fl/ unboxed-gensym-1 unboxed-gensym-2))
             (unboxed-gensym-8 0.0)
             (unboxed-gensym-10 (unsafe-fl+ (unsafe-fl* unboxed-gensym-3
                                                        unboxed-gensym-3)
                                            (unsafe-fl* unboxed-gensym-4
                                                        unboxed-gensym-4)))
             (unboxed-gensym-5 (unsafe-fl/ (unsafe-fl+ (unsafe-fl* unboxed-gensym-7
                                                                   unboxed-gensym-3)
                                                       (unsafe-fl* unboxed-gensym-8
                                                                   unboxed-gensym-4))
                                           unboxed-gensym-10))
             (unboxed-gensym-6 (unsafe-fl/ (unsafe-fl- (unsafe-fl* unboxed-gensym-8
                                                                   unboxed-gensym-3)
                                                       (unsafe-fl* unboxed-gensym-7
                                                                   unboxed-gensym-4))
                                           unboxed-gensym-10)))
        (unsafe-make-flrectangular unboxed-gensym-5 unboxed-gensym-6))))
