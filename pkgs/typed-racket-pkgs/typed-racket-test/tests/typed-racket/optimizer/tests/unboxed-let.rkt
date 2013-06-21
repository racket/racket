#;
#<<END
TR opt: unboxed-let.rkt 35:14 t1 -- unbox float-complex
TR opt: unboxed-let.rkt 35:17 3.0+6.0i -- unboxed literal
TR opt: unboxed-let.rkt 35:11 (- t1 3.0+6.0i) -- unboxed binary float complex
TR opt: unboxed-let.rkt 34:14 1.0+2.0i -- unboxed literal
TR opt: unboxed-let.rkt 34:23 2.0+4.0i -- unboxed literal
TR opt: unboxed-let.rkt 34:11 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: unboxed-let.rkt 34:0 (let* ((t1 (+ 1.0+2.0i 2.0+4.0i)) (t2 (- t1 3.0+6.0i)) (t3 4.0+8.0i)) (+ t2 t3)) -- unboxed let bindings
TR opt: unboxed-let.rkt 37:5 t2 -- unbox float-complex
TR opt: unboxed-let.rkt 37:8 t3 -- unbox float-complex
TR opt: unboxed-let.rkt 37:2 (+ t2 t3) -- unboxed binary float complex
TR opt: unboxed-let.rkt 35:14 t1 -- leave var unboxed
TR opt: unboxed-let.rkt 35:17 3.0+6.0i -- unboxed literal
TR opt: unboxed-let.rkt 35:11 (- t1 3.0+6.0i) -- unboxed binary float complex
TR opt: unboxed-let.rkt 34:0 (let* ((t1 (+ 1.0+2.0i 2.0+4.0i)) (t2 (- t1 3.0+6.0i)) (t3 4.0+8.0i)) (+ t2 t3)) -- unboxed let bindings
TR opt: unboxed-let.rkt 37:5 t2 -- leave var unboxed
TR opt: unboxed-let.rkt 37:8 t3 -- unbox float-complex
TR opt: unboxed-let.rkt 37:2 (+ t2 t3) -- unboxed binary float complex
TR opt: unboxed-let.rkt 36:11 4.0+8.0i -- unboxed literal
TR opt: unboxed-let.rkt 34:0 (let* ((t1 (+ 1.0+2.0i 2.0+4.0i)) (t2 (- t1 3.0+6.0i)) (t3 4.0+8.0i)) (+ t2 t3)) -- unboxed let bindings
TR opt: unboxed-let.rkt 37:5 t2 -- leave var unboxed
TR opt: unboxed-let.rkt 37:8 t3 -- leave var unboxed
TR opt: unboxed-let.rkt 37:2 (+ t2 t3) -- unboxed binary float complex
4.0+8.0i

END

#lang typed/scheme
#:optimize



(let* ((t1 (+ 1.0+2.0i 2.0+4.0i))
       (t2 (- t1 3.0+6.0i))
       (t3 4.0+8.0i))
  (+ t2 t3))
