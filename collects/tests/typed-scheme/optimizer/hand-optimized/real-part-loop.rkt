#lang racket/base 

(require racket/unsafe/ops)

(let-values (((unboxed-real-1) '0.0))
  (let-values (((unboxed-imag-2) '1.0))
    ((letrec-values (((loop)
                      (lambda (unboxed-real-1 unboxed-imag-2)
                        (if (unsafe-fl> (let-values () unboxed-real-1) '70000.2)
                            '0
                            (let-values (((unboxed-float-1) '3.6))
                              (let-values (((unboxed-real-2) (unsafe-fl+ unboxed-real-1 unboxed-float-1)))
                                (let-values (((unboxed-imag-3) unboxed-imag-2)) (loop unboxed-real-2 unboxed-imag-3))))))))
       loop)
     unboxed-real-1
     unboxed-imag-2)))

(void)

