#;#;
#<<END
TR opt: unboxed-let2.rkt 28:0 (let ((t1 (+ 1.0+2.0i 2.0+4.0i)) (t2 (+ 3.0+6.0i 4.0+8.0i))) (+ t1 t2)) -- unboxed let bindings
TR opt: unboxed-let2.rkt 28:10 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: unboxed-let2.rkt 28:13 1.0+2.0i -- unboxed literal
TR opt: unboxed-let2.rkt 28:22 2.0+4.0i -- unboxed literal
TR opt: unboxed-let2.rkt 29:10 (+ 3.0+6.0i 4.0+8.0i) -- unboxed binary float complex
TR opt: unboxed-let2.rkt 29:13 3.0+6.0i -- unboxed literal
TR opt: unboxed-let2.rkt 29:22 4.0+8.0i -- unboxed literal
TR opt: unboxed-let2.rkt 30:2 (+ t1 t2) -- unboxed binary float complex
TR opt: unboxed-let2.rkt 30:2 (+ t1 t2) -- unboxed binary float complex
TR opt: unboxed-let2.rkt 30:2 (+ t1 t2) -- unboxed binary float complex
TR opt: unboxed-let2.rkt 30:5 t1 -- leave var unboxed
TR opt: unboxed-let2.rkt 30:5 t1 -- unbox float-complex
TR opt: unboxed-let2.rkt 30:5 t1 -- unbox float-complex
TR opt: unboxed-let2.rkt 30:8 t2 -- leave var unboxed
TR opt: unboxed-let2.rkt 30:8 t2 -- unbox float-complex
TR opt: unboxed-let2.rkt 30:8 t2 -- unbox float-complex
END
#<<END
10.0+20.0i

END

#lang typed/scheme
#:optimize

(let ((t1 (+ 1.0+2.0i 2.0+4.0i))
      (t2 (+ 3.0+6.0i 4.0+8.0i)))
  (+ t1 t2))
