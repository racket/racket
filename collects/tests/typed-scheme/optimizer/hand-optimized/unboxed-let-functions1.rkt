#lang racket

(require racket/unsafe/ops)

;; simple case, function with single complex arg
(let ((f (lambda (unboxed-real-1 unboxed-imag-2)
           (let*-values (((unboxed-gensym-3) 3.0)
                         ((unboxed-gensym-4) 6.0)
                         ((unboxed-gensym-5) (unsafe-fl+ unboxed-real-1 unboxed-gensym-3))
                         ((unboxed-gensym-6) (unsafe-fl+ unboxed-imag-2 unboxed-gensym-4)))
             (unsafe-make-flrectangular unboxed-gensym-5 unboxed-gensym-6)))))
  (let*-values (((unboxed-gensym-1) 1.0)
                ((unboxed-gensym-2) 2.0)
                ((unboxed-gensym-3) 2.0)
                ((unboxed-gensym-4) 4.0)
                ((unboxed-gensym-5) (unsafe-fl+ unboxed-gensym-1 unboxed-gensym-3))
                ((unboxed-gensym-6) (unsafe-fl+ unboxed-gensym-2 unboxed-gensym-4)))
    (f unboxed-gensym-5 unboxed-gensym-6)))
(void)
