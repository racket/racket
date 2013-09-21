#;#;
#<<END
TR info: unboxed-let-constants-fail1.rkt 22:22 displayln -- hidden parameter
TR opt: unboxed-let-constants-fail1.rkt 21:0 (letrec-values (((x) 5.0+5.0i) ((_) (displayln (exact? x))) ((z) x)) (real-part (+ x z))) -- unboxed let bindings
TR opt: unboxed-let-constants-fail1.rkt 21:21 5.0+5.0i -- unboxed literal
TR opt: unboxed-let-constants-fail1.rkt 22:40 x -- unboxed complex variable
TR opt: unboxed-let-constants-fail1.rkt 23:21 x -- leave var unboxed
TR opt: unboxed-let-constants-fail1.rkt 24:13 (+ x z) -- unboxed binary float complex
TR opt: unboxed-let-constants-fail1.rkt 24:16 x -- leave var unboxed
TR opt: unboxed-let-constants-fail1.rkt 24:18 z -- leave var unboxed
TR opt: unboxed-let-constants-fail1.rkt 24:2 (real-part (+ x z)) -- complex accessor elimination
END
#<<END
#f
10.0

END
#lang typed/racket 


(letrec-values (((x) 5.0+5.0i)
                ((_) (displayln (exact? x)))
                ((z) x))
  (real-part (+ x z)))
