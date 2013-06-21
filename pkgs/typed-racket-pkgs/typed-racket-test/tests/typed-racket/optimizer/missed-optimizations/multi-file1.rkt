#;#;
#<<END
TR info: multi-file1.rkt 13:2 (* x (ann 3 Integer)) -- exact real arith
TR missed opt: multi-file1.rkt 13:2 (* x (ann 3 Integer)) -- all args float-arg-expr, result not Float -- caused by: 13:12 3
END
""

#lang typed/racket
(provide f)

(: f : Float -> Real)
(define (f x)
  (* x (ann 3 Integer)))
