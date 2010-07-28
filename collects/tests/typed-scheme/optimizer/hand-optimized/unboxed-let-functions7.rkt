#lang racket

(require racket/unsafe/ops racket/flonum)

(let*-values (((unboxed-gensym-1) 0.0+0.0i)
              ((unboxed-gensym-2) (unsafe-flreal-part unboxed-gensym-1))
              ((unboxed-gensym-3) (unsafe-flimag-part unboxed-gensym-1)))
  ((letrec-values
    (((loop)
      (lambda (unboxed-real-1 unboxed-imag-2 l)
        (if (null? l)
            (unsafe-make-flrectangular unboxed-real-1 unboxed-imag-2)
            (let*-values (((unboxed-gensym-3) (->fl (unsafe-car l)))
                          ((unboxed-gensym-4) (unsafe-fl+ unboxed-real-1 unboxed-gensym-3))
                          ((unboxed-gensym-5) unboxed-imag-2))
              (loop unboxed-gensym-4 unboxed-gensym-5
                    (unsafe-cdr l)))))))
    loop)
   unboxed-gensym-2 unboxed-gensym-3 '(1 2 3)))
(void)
