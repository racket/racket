#lang racket

(require racket/unsafe/ops)

(let*-values (((unboxed-real-1) 0.0)
              ((unboxed-imag-2) 0.0))
  ((letrec-values
    (((loop1)
      (lambda (unboxed-real-1 unboxed-imag-2 x)
        (if (null? x)
            (unsafe-make-flrectangular unboxed-real-1
                                       unboxed-imag-2)
            (let*-values (((unboxed-real-1) '0.0)
                          ((unboxed-imag-2) '0.0))
              ((letrec-values
                (((loop2)
                  (lambda (unboxed-real-1 unboxed-imag-2 y)
                    (if (null? y)
                        (let*-values (((unboxed-real-3)
                                       (unsafe-fl+ unboxed-real-1
                                                   unboxed-real-1))
                                      ((unboxed-imag-4)
                                       (unsafe-fl+ unboxed-imag-2
                                                   unboxed-imag-2)))
                          (loop1 unboxed-real-3 unboxed-imag-4 (unsafe-cdr x)))
                        (let*-values (((unboxed-gensym-1) (unsafe-car x))
                                      ((unboxed-real-2)
                                       (unsafe-flreal-part unboxed-gensym-1))
                                      ((unboxed-imag-3)
                                       (unsafe-flimag-part unboxed-gensym-1))
                                      ((unboxed-gensym-4) (unsafe-car y))
                                      ((unboxed-real-5)
                                       (unsafe-flreal-part unboxed-gensym-4))
                                      ((unboxed-imag-6)
                                       (unsafe-flimag-part unboxed-gensym-4))
                                      ((unboxed-real-7)
                                       (unsafe-fl+ (unsafe-fl+ unboxed-real-1
                                                               unboxed-real-2)
                                                   unboxed-real-5))
                                      ((unboxed-imag-8)
                                       (unsafe-fl+ (unsafe-fl+ unboxed-imag-2
                                                               unboxed-imag-3)
                                                   unboxed-imag-6)))
                          (loop2 unboxed-real-7 unboxed-imag-8
                                 (unsafe-cdr y)))))))
                loop2)
               unboxed-real-1
               unboxed-imag-2
               '(3.0+6.0i 4.0+8.0i)))))))
    loop1)
   unboxed-real-1
   unboxed-imag-2
   '(1.0+2.0i 2.0+4.0i)))
(void)
