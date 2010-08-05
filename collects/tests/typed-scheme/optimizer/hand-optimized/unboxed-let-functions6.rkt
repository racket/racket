#lang racket

(require racket/unsafe/ops racket/flonum)

(let*-values (((unboxed-real-1) 0.0)
              ((unboxed-imag-2) 0.0))
  ((letrec-values
    (((loop)
      (lambda (unboxed-real-1 unboxed-imag-2 l)
        (if (null? l)
            (let*-values (((unboxed-real-3) 0.0)
                          ((unboxed-imag-4) 1.0)
                          ((unboxed-real-5) (unsafe-fl+ unboxed-real-1 unboxed-real-3))
                          ((unboxed-imag-6) (unsafe-fl+ unboxed-imag-2 unboxed-imag-4)))
              (unsafe-make-flrectangular unboxed-real-5 unboxed-imag-6))
            (let*-values (((unboxed-float-1) (->fl (unsafe-car l)))
                          ((unboxed-real-2) (unsafe-fl+ unboxed-real-1 unboxed-float-1))
                          ((unboxed-imag-3) unboxed-imag-2))
              (loop unboxed-real-2 unboxed-imag-3
                    (unsafe-cdr l)))))))
    loop)
   unboxed-real-1 unboxed-imag-2 '(1 2 3)))
(void)
