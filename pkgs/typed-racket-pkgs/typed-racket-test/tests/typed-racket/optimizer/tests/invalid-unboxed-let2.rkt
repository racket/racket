#;#;
#<<END
TR opt: invalid-unboxed-let2.rkt 5:30 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: invalid-unboxed-let2.rkt 5:33 1.0+2.0i -- unboxed literal
TR opt: invalid-unboxed-let2.rkt 5:42 2.0+4.0i -- unboxed literal
TR opt: invalid-unboxed-let2.rkt 5:52 (+ 3.0+6.0i 4.0+8.0i) -- unboxed binary float complex
TR opt: invalid-unboxed-let2.rkt 5:55 3.0+6.0i -- unboxed literal
TR opt: invalid-unboxed-let2.rkt 5:64 4.0+8.0i -- unboxed literal
TR opt: invalid-unboxed-let2.rkt 6:2 (+ t1 t2) -- unboxed binary float complex
TR opt: invalid-unboxed-let2.rkt 6:5 t1 -- unbox float-complex
TR opt: invalid-unboxed-let2.rkt 6:8 t2 -- unbox float-complex
END
#<<END
10.0+20.0i

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port



;; unboxing of let bindings does not currently work with multiple values
(let-values (((t1 t2) (values (+ 1.0+2.0i 2.0+4.0i) (+ 3.0+6.0i 4.0+8.0i))))
  (+ t1 t2))
