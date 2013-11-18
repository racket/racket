#;#;
#<<END
TR opt: unboxed-let.rkt 27:11 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: unboxed-let.rkt 27:14 1.0+2.0i -- unboxed literal
TR opt: unboxed-let.rkt 27:23 2.0+4.0i -- unboxed literal
TR opt: unboxed-let.rkt 27:7 (t1 (+ 1.0+2.0i 2.0+4.0i)) -- unboxed let bindings
TR opt: unboxed-let.rkt 28:11 (- t1 3.0+6.0i) -- unboxed binary float complex
TR opt: unboxed-let.rkt 28:14 t1 -- leave var unboxed
TR opt: unboxed-let.rkt 28:17 3.0+6.0i -- unboxed literal
TR opt: unboxed-let.rkt 28:7 (t2 (- t1 3.0+6.0i)) -- unboxed let bindings
TR opt: unboxed-let.rkt 29:11 4.0+8.0i -- unboxed literal
TR opt: unboxed-let.rkt 29:7 (t3 4.0+8.0i) -- unboxed let bindings
TR opt: unboxed-let.rkt 30:2 (+ t2 t3) -- unboxed binary float complex
TR opt: unboxed-let.rkt 30:5 t2 -- leave var unboxed
TR opt: unboxed-let.rkt 30:8 t3 -- leave var unboxed
END
#<<END
4.0+8.0i

END

#lang typed/scheme
#:optimize



(let* ((t1 (+ 1.0+2.0i 2.0+4.0i))
       (t2 (- t1 3.0+6.0i))
       (t3 4.0+8.0i))
  (+ t2 t3))
