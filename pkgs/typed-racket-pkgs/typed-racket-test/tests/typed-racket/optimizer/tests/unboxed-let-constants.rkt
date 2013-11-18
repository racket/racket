#;#;
#<<END
TR opt: unboxed-let-constants.rkt 19:16 ((x) 5.0+5.0i) -- unboxed let bindings
TR opt: unboxed-let-constants.rkt 19:21 5.0+5.0i -- unboxed literal
TR opt: unboxed-let-constants.rkt 20:16 ((z) x) -- unboxed let bindings
TR opt: unboxed-let-constants.rkt 20:21 x -- leave var unboxed
TR opt: unboxed-let-constants.rkt 21:13 (+ x z) -- unboxed binary float complex
TR opt: unboxed-let-constants.rkt 21:16 x -- leave var unboxed
TR opt: unboxed-let-constants.rkt 21:18 z -- leave var unboxed
TR opt: unboxed-let-constants.rkt 21:2 (real-part (+ x z)) -- complex accessor elimination
END
#<<END
10.0

END
#lang typed/racket


(letrec-values (((x) 5.0+5.0i)
                ((z) x))
  (real-part (+ x z)))
