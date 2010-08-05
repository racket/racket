#lang racket

(require racket/unsafe/ops)

(letrec-syntaxes+values
 (((s) (syntax-rules () [(_ x) x])))
 (((unboxed-gensym-1) 1.0)
  ((unboxed-gensym-2) 2.0)
  ((unboxed-gensym-3) 2.0)
  ((unboxed-gensym-4) 4.0)
  ((unboxed-gensym-5) (unsafe-fl+ unboxed-gensym-1 unboxed-gensym-3))
  ((unboxed-gensym-6) (unsafe-fl+ unboxed-gensym-2 unboxed-gensym-4)))
 (let* ((unboxed-gensym-7 2.0)
        (unboxed-gensym-8 4.0)
        (unboxed-gensym-9 (unsafe-fl+ unboxed-gensym-5 unboxed-gensym-7))
        (unboxed-gensym-10 (unsafe-fl+ unboxed-gensym-6 unboxed-gensym-8)))
   (unsafe-make-flrectangular unboxed-gensym-9 unboxed-gensym-10)))
(void)
