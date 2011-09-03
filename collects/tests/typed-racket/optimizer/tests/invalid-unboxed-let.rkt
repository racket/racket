#;
(
TR opt: invalid-unboxed-let.rkt 20:0 (let ((t1 (+ 1.0+2.0i 2.0+4.0i)) (t2 (+ 3.0+6.0i 4.0+8.0i)) (t3 1.0+2.0i) (t4 1)) (display (+ t1 t1)) (display t2) (display t3) (display t4)) -- unboxed let bindings
TR opt: invalid-unboxed-let.rkt 20:10 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: invalid-unboxed-let.rkt 20:13 1.0+2.0i -- unboxed literal
TR opt: invalid-unboxed-let.rkt 20:22 2.0+4.0i -- unboxed literal
TR opt: invalid-unboxed-let.rkt 21:10 (+ 3.0+6.0i 4.0+8.0i) -- unboxed binary float complex
TR opt: invalid-unboxed-let.rkt 21:13 3.0+6.0i -- unboxed literal
TR opt: invalid-unboxed-let.rkt 21:22 4.0+8.0i -- unboxed literal
TR opt: invalid-unboxed-let.rkt 24:11 (+ t1 t1) -- unboxed binary float complex
TR opt: invalid-unboxed-let.rkt 24:14 t1 -- leave var unboxed
TR opt: invalid-unboxed-let.rkt 24:14 t1 -- unbox float-complex
TR opt: invalid-unboxed-let.rkt 24:17 t1 -- leave var unboxed
TR opt: invalid-unboxed-let.rkt 24:17 t1 -- unbox float-complex
6.0+12.0i7.0+14.0i1.0+2.0i1)

#lang typed/scheme
#:optimize

(let ((t1 (+ 1.0+2.0i 2.0+4.0i)) ; can be unboxed
      (t2 (+ 3.0+6.0i 4.0+8.0i)) ; can't be unboxed
      (t3 1.0+2.0i) ; can't be unboxed
      (t4 1))
  (display (+ t1 t1))
  (display t2)
  (display t3)
  (display t4))
