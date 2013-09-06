#;#;
#<<END
TR opt: unboxed-let2.rkt 22:0 (let ((t1 (+ 1.0+2.0i 2.0+4.0i)) (t2 (+ 3.0+6.0i 4.0+8.0i))) (+ t1 t2)) -- unboxed let bindings
TR opt: unboxed-let2.rkt 22:10 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: unboxed-let2.rkt 22:13 1.0+2.0i -- unboxed literal
TR opt: unboxed-let2.rkt 22:22 2.0+4.0i -- unboxed literal
TR opt: unboxed-let2.rkt 23:10 (+ 3.0+6.0i 4.0+8.0i) -- unboxed binary float complex
TR opt: unboxed-let2.rkt 23:13 3.0+6.0i -- unboxed literal
TR opt: unboxed-let2.rkt 23:22 4.0+8.0i -- unboxed literal
TR opt: unboxed-let2.rkt 24:2 (+ t1 t2) -- unboxed binary float complex
TR opt: unboxed-let2.rkt 24:5 t1 -- leave var unboxed
TR opt: unboxed-let2.rkt 24:8 t2 -- leave var unboxed
END
#<<END
10.0+20.0i

END

#lang typed/scheme
#:optimize

(let ((t1 (+ 1.0+2.0i 2.0+4.0i))
      (t2 (+ 3.0+6.0i 4.0+8.0i)))
  (+ t1 t2))
