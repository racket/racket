#;#;
#<<END
TR opt: nested-unboxed-let.rkt 23:12 1.0+2.0i -- unboxed literal
TR opt: nested-unboxed-let.rkt 23:21 2.0+3.0i -- unboxed literal
TR opt: nested-unboxed-let.rkt 23:6 (x (+ 1.0+2.0i 2.0+3.0i)) -- unboxed let bindings
TR opt: nested-unboxed-let.rkt 23:9 (+ 1.0+2.0i 2.0+3.0i) -- unboxed binary float complex
TR opt: nested-unboxed-let.rkt 24:11 (+ x 2.0+3.0i) -- unboxed binary float complex
TR opt: nested-unboxed-let.rkt 24:14 x -- leave var unboxed
TR opt: nested-unboxed-let.rkt 24:16 2.0+3.0i -- unboxed literal
TR opt: nested-unboxed-let.rkt 24:8 (x (+ x 2.0+3.0i)) -- unboxed let bindings
TR opt: nested-unboxed-let.rkt 25:4 (+ x 3.0+6.0i) -- unboxed binary float complex
TR opt: nested-unboxed-let.rkt 25:7 x -- leave var unboxed
TR opt: nested-unboxed-let.rkt 25:9 3.0+6.0i -- unboxed literal
END
#<<END
8.0+14.0i

END

#lang typed/scheme
#:optimize

(let ((x (+ 1.0+2.0i 2.0+3.0i)))
  (let ((x (+ x 2.0+3.0i)))
    (+ x 3.0+6.0i)))
