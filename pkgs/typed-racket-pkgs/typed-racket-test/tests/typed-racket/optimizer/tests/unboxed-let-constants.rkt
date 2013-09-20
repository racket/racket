#;#;
#<<END
TR opt: unboxed-let-constants.rkt 18:0 (letrec-values (((x) 5.0+5.0i) ((z) x)) (real-part (+ x z))) -- unboxed let bindings
TR opt: unboxed-let-constants.rkt 18:21 5.0+5.0i -- unboxed literal
TR opt: unboxed-let-constants.rkt 19:21 x -- leave var unboxed
TR opt: unboxed-let-constants.rkt 20:13 (+ x z) -- unboxed binary float complex
TR opt: unboxed-let-constants.rkt 20:16 x -- leave var unboxed
TR opt: unboxed-let-constants.rkt 20:18 z -- leave var unboxed
TR opt: unboxed-let-constants.rkt 20:2 (real-part (+ x z)) -- complex accessor elimination
END
#<<END
10.0

END
#lang typed/racket


(letrec-values (((x) 5.0+5.0i)
                ((z) x))
  (real-part (+ x z)))
