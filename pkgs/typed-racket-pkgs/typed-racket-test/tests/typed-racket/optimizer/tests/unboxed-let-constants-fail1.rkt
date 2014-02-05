#;#;
#<<END
TR info: unboxed-let-constants-fail1.rkt 4:22 displayln -- hidden parameter
TR opt: unboxed-let-constants-fail1.rkt 3:16 ((x) 5.0+5.0i) -- unboxed let bindings
TR opt: unboxed-let-constants-fail1.rkt 3:21 5.0+5.0i -- unboxed literal
TR opt: unboxed-let-constants-fail1.rkt 4:40 x -- unboxed complex variable
TR opt: unboxed-let-constants-fail1.rkt 5:16 ((z) x) -- unboxed let bindings
TR opt: unboxed-let-constants-fail1.rkt 5:21 x -- leave var unboxed
TR opt: unboxed-let-constants-fail1.rkt 6:13 (+ x z) -- unboxed binary float complex
TR opt: unboxed-let-constants-fail1.rkt 6:16 x -- leave var unboxed
TR opt: unboxed-let-constants-fail1.rkt 6:18 z -- leave var unboxed
TR opt: unboxed-let-constants-fail1.rkt 6:2 (real-part (+ x z)) -- complex accessor elimination
END
#<<END
#f
10.0

END
#lang typed/racket
#reader tests/typed-racket/optimizer/reset-port


(letrec-values (((x) 5.0+5.0i)
                ((_) (displayln (exact? x)))
                ((z) x))
  (real-part (+ x z)))
