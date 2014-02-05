#;#;
#<<END
TR info: invalid-unboxed-let.rkt 6:3 display -- hidden parameter
TR info: invalid-unboxed-let.rkt 7:3 display -- hidden parameter
TR info: invalid-unboxed-let.rkt 8:3 display -- hidden parameter
TR info: invalid-unboxed-let.rkt 9:3 display -- hidden parameter
TR opt: invalid-unboxed-let.rkt 2:10 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: invalid-unboxed-let.rkt 2:13 1.0+2.0i -- unboxed literal
TR opt: invalid-unboxed-let.rkt 2:22 2.0+4.0i -- unboxed literal
TR opt: invalid-unboxed-let.rkt 2:6 (t1 (+ 1.0+2.0i 2.0+4.0i)) -- unboxed let bindings
TR opt: invalid-unboxed-let.rkt 3:10 (+ 3.0+6.0i 4.0+8.0i) -- unboxed binary float complex
TR opt: invalid-unboxed-let.rkt 3:13 3.0+6.0i -- unboxed literal
TR opt: invalid-unboxed-let.rkt 3:22 4.0+8.0i -- unboxed literal
TR opt: invalid-unboxed-let.rkt 6:11 (+ t1 t1) -- unboxed binary float complex
TR opt: invalid-unboxed-let.rkt 6:14 t1 -- leave var unboxed
TR opt: invalid-unboxed-let.rkt 6:17 t1 -- leave var unboxed
END
"6.0+12.0i7.0+14.0i1.0+2.0i1"
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(let ((t1 (+ 1.0+2.0i 2.0+4.0i)) ; can be unboxed
      (t2 (+ 3.0+6.0i 4.0+8.0i)) ; can't be unboxed
      (t3 1.0+2.0i) ; can't be unboxed
      (t4 1))
  (display (+ t1 t1))
  (display t2)
  (display t3)
  (display t4))
