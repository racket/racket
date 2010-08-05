#lang racket

(require racket/unsafe/ops)

(let-values (((pos->vals pos-next init pos-cont? val-cont? all-cont?)
              (let-values (((i) '(1.0+2.0i 2.0+4.0i)))
                (values
                 unsafe-car
                 unsafe-cdr
                 i
                 (lambda (x) (not (null? x)))
                 (lambda (x) #t)
                 (lambda (x y) #t)))))
  (void)
  (let*-values (((unboxed-real-1) 0.0)
                ((unboxed-imag-2) 0.0))
    ((letrec-values
      (((for-loop)
        (lambda (unboxed-real-1 unboxed-imag-2 pos)
          (if (pos-cont? pos)
              (let*-values (((unboxed-gensym-1) (pos->vals pos))
                            ((unboxed-real-2) (unsafe-flreal-part unboxed-gensym-1))
                            ((unboxed-imag-3) (unsafe-flimag-part unboxed-gensym-1)))
                (if (val-cont? (unsafe-make-flrectangular unboxed-real-2 unboxed-imag-3))
                    (let-values (((sum)
                                  (let-values ()
                                    (let-values ()
                                      (let*-values (((unboxed-real-1) (unsafe-fl+ unboxed-real-2 unboxed-real-1))
                                                    ((unboxed-imag-2) (unsafe-fl+ unboxed-imag-3 unboxed-imag-2)))
                                        (unsafe-make-flrectangular  unboxed-real-1 unboxed-imag-2))))))
                      (if (all-cont? pos (unsafe-make-flrectangular unboxed-real-2 unboxed-imag-3))
                          (let*-values (((unboxed-gensym-1) sum)
                                        ((unboxed-real-2) (unsafe-flreal-part unboxed-gensym-1))
                                        ((unboxed-imag-3) (unsafe-flimag-part unboxed-gensym-1)))
                            (for-loop unboxed-real-2 unboxed-imag-3 (pos-next pos)))
                          sum))
                    (unsafe-make-flrectangular unboxed-real-1 unboxed-imag-2)))
              (unsafe-make-flrectangular unboxed-real-1 unboxed-imag-2)))))
      for-loop)
     unboxed-real-1
     unboxed-imag-2
     init)))
(void)
