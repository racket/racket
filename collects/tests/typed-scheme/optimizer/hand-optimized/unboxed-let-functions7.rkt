#lang racket

(require racket/unsafe/ops racket/flonum)

(let*-values (((unboxed-real-1) 0.0)
              ((unboxed-imag-2) 0.0))
  ((letrec-values
    (((loop)
      (lambda (unboxed-real-1 unboxed-imag-2 l)
        (if (null? l)
            (unsafe-make-flrectangular unboxed-real-1 unboxed-imag-2)
            (let*-values (((unboxed-float-3) (->fl (unsafe-car l)))
                          ((unboxed-real-4) (unsafe-fl+ unboxed-real-1 unboxed-float-3))
                          ((unboxed-imag-5) unboxed-imag-2))
              (loop unboxed-real-4 unboxed-imag-5
                    (unsafe-cdr l)))))))
    loop)
   unboxed-real-1 unboxed-imag-2 '(1 2 3)))
(void)
