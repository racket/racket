#;#;
#<<END
TR opt: unboxed-let-constants.rkt 3:16 ((x) 5.0+5.0i) -- unboxed let bindings
TR opt: unboxed-let-constants.rkt 3:21 5.0+5.0i -- unboxed literal
TR opt: unboxed-let-constants.rkt 4:16 ((z) x) -- unboxed let bindings
TR opt: unboxed-let-constants.rkt 4:21 x -- leave var unboxed
TR opt: unboxed-let-constants.rkt 5:13 (+ x z) -- unboxed binary float complex
TR opt: unboxed-let-constants.rkt 5:16 x -- leave var unboxed
TR opt: unboxed-let-constants.rkt 5:18 z -- leave var unboxed
TR opt: unboxed-let-constants.rkt 5:2 (real-part (+ x z)) -- complex accessor elimination
END
#<<END
10.0

END
#lang typed/racket
#reader tests/typed-racket/optimizer/reset-port


(letrec-values (((x) 5.0+5.0i)
                ((z) x))
  (real-part (+ x z)))
