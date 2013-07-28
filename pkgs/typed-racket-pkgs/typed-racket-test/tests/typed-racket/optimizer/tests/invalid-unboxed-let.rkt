#;
(
TR opt: invalid-unboxed-let.rkt 35:14 t1 -- unbox float-complex
TR opt: invalid-unboxed-let.rkt 35:17 t1 -- unbox float-complex
TR opt: invalid-unboxed-let.rkt 35:11 (+ t1 t1) -- unboxed binary float complex
TR opt: invalid-unboxed-let.rkt 35:14 t1 -- unbox float-complex
TR opt: invalid-unboxed-let.rkt 35:17 t1 -- unbox float-complex
TR opt: invalid-unboxed-let.rkt 35:11 (+ t1 t1) -- unboxed binary float complex
TR opt: invalid-unboxed-let.rkt 35:14 t1 -- unbox float-complex
TR opt: invalid-unboxed-let.rkt 35:17 t1 -- unbox float-complex
TR opt: invalid-unboxed-let.rkt 35:11 (+ t1 t1) -- unboxed binary float complex
TR opt: invalid-unboxed-let.rkt 31:13 1.0+2.0i -- unboxed literal
TR opt: invalid-unboxed-let.rkt 31:22 2.0+4.0i -- unboxed literal
TR opt: invalid-unboxed-let.rkt 31:10 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: invalid-unboxed-let.rkt 32:13 3.0+6.0i -- unboxed literal
TR opt: invalid-unboxed-let.rkt 32:22 4.0+8.0i -- unboxed literal
TR opt: invalid-unboxed-let.rkt 32:10 (+ 3.0+6.0i 4.0+8.0i) -- unboxed binary float complex
TR opt: invalid-unboxed-let.rkt 31:0 (let ((t1 (+ 1.0+2.0i 2.0+4.0i)) (t2 (+ 3.0+6.0i 4.0+8.0i)) (t3 1.0+2.0i) (t4 1)) (display (+ t1 t1)) (display t2) (display t3) (display t4)) -- unboxed let bindings
TR info: invalid-unboxed-let.rkt 35:3 display -- hidden parameter
TR opt: invalid-unboxed-let.rkt 35:14 t1 -- leave var unboxed
TR opt: invalid-unboxed-let.rkt 35:17 t1 -- leave var unboxed
TR opt: invalid-unboxed-let.rkt 35:11 (+ t1 t1) -- unboxed binary float complex
TR info: invalid-unboxed-let.rkt 36:3 display -- hidden parameter
TR info: invalid-unboxed-let.rkt 37:3 display -- hidden parameter
TR info: invalid-unboxed-let.rkt 38:3 display -- hidden parameter
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
