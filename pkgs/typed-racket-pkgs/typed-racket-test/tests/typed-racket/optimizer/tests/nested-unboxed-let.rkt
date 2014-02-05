#;#;
#<<END
TR opt: nested-unboxed-let.rkt 2:12 1.0+2.0i -- unboxed literal
TR opt: nested-unboxed-let.rkt 2:21 2.0+3.0i -- unboxed literal
TR opt: nested-unboxed-let.rkt 2:6 (x (+ 1.0+2.0i 2.0+3.0i)) -- unboxed let bindings
TR opt: nested-unboxed-let.rkt 2:9 (+ 1.0+2.0i 2.0+3.0i) -- unboxed binary float complex
TR opt: nested-unboxed-let.rkt 3:11 (+ x 2.0+3.0i) -- unboxed binary float complex
TR opt: nested-unboxed-let.rkt 3:14 x -- leave var unboxed
TR opt: nested-unboxed-let.rkt 3:16 2.0+3.0i -- unboxed literal
TR opt: nested-unboxed-let.rkt 3:8 (x (+ x 2.0+3.0i)) -- unboxed let bindings
TR opt: nested-unboxed-let.rkt 4:4 (+ x 3.0+6.0i) -- unboxed binary float complex
TR opt: nested-unboxed-let.rkt 4:7 x -- leave var unboxed
TR opt: nested-unboxed-let.rkt 4:9 3.0+6.0i -- unboxed literal
END
#<<END
8.0+14.0i

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(let ((x (+ 1.0+2.0i 2.0+3.0i)))
  (let ((x (+ x 2.0+3.0i)))
    (+ x 3.0+6.0i)))
