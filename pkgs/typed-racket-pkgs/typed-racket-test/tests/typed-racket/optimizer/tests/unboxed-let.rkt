#;#;
#<<END
TR opt: unboxed-let.rkt 2:11 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: unboxed-let.rkt 2:14 1.0+2.0i -- unboxed literal
TR opt: unboxed-let.rkt 2:23 2.0+4.0i -- unboxed literal
TR opt: unboxed-let.rkt 2:7 (t1 (+ 1.0+2.0i 2.0+4.0i)) -- unboxed let bindings
TR opt: unboxed-let.rkt 3:11 (- t1 3.0+6.0i) -- unboxed binary float complex
TR opt: unboxed-let.rkt 3:14 t1 -- leave var unboxed
TR opt: unboxed-let.rkt 3:17 3.0+6.0i -- unboxed literal
TR opt: unboxed-let.rkt 3:7 (t2 (- t1 3.0+6.0i)) -- unboxed let bindings
TR opt: unboxed-let.rkt 4:11 4.0+8.0i -- unboxed literal
TR opt: unboxed-let.rkt 4:7 (t3 4.0+8.0i) -- unboxed let bindings
TR opt: unboxed-let.rkt 5:2 (+ t2 t3) -- unboxed binary float complex
TR opt: unboxed-let.rkt 5:5 t2 -- leave var unboxed
TR opt: unboxed-let.rkt 5:8 t3 -- leave var unboxed
END
#<<END
4.0+8.0i

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(let* ((t1 (+ 1.0+2.0i 2.0+4.0i))
       (t2 (- t1 3.0+6.0i))
       (t3 4.0+8.0i))
  (+ t2 t3))
