#;#;
#<<END
TR opt: nested-unboxed-let.rkt 29:0 (let ((x (+ 1.0+2.0i 2.0+3.0i))) (let ((x (+ x 2.0+3.0i))) (+ x 3.0+6.0i))) -- unboxed let bindings
TR opt: nested-unboxed-let.rkt 29:12 1.0+2.0i -- unboxed literal
TR opt: nested-unboxed-let.rkt 29:21 2.0+3.0i -- unboxed literal
TR opt: nested-unboxed-let.rkt 29:9 (+ 1.0+2.0i 2.0+3.0i) -- unboxed binary float complex
TR opt: nested-unboxed-let.rkt 30:11 (+ x 2.0+3.0i) -- unboxed binary float complex
TR opt: nested-unboxed-let.rkt 30:11 (+ x 2.0+3.0i) -- unboxed binary float complex
TR opt: nested-unboxed-let.rkt 30:14 x -- leave var unboxed
TR opt: nested-unboxed-let.rkt 30:14 x -- unbox float-complex
TR opt: nested-unboxed-let.rkt 30:16 2.0+3.0i -- unboxed literal
TR opt: nested-unboxed-let.rkt 30:16 2.0+3.0i -- unboxed literal
TR opt: nested-unboxed-let.rkt 30:2 (let ((x (+ x 2.0+3.0i))) (+ x 3.0+6.0i)) -- unboxed let bindings
TR opt: nested-unboxed-let.rkt 31:4 (+ x 3.0+6.0i) -- unboxed binary float complex
TR opt: nested-unboxed-let.rkt 31:4 (+ x 3.0+6.0i) -- unboxed binary float complex
TR opt: nested-unboxed-let.rkt 31:7 x -- leave var unboxed
TR opt: nested-unboxed-let.rkt 31:7 x -- unbox float-complex
TR opt: nested-unboxed-let.rkt 31:9 3.0+6.0i -- unboxed literal
TR opt: nested-unboxed-let.rkt 31:9 3.0+6.0i -- unboxed literal
END
#<<END
8.0+14.0i

END

#lang typed/scheme
#:optimize

(let ((x (+ 1.0+2.0i 2.0+3.0i)))
  (let ((x (+ x 2.0+3.0i)))
    (+ x 3.0+6.0i)))
