#;#;
#<<END
TR missed opt: multi-file1.rkt 14:2 (* x (ann 3 Integer)) -- all args float-arg-expr, result not Float -- caused by: 14:12 3
TR info: multi-file1.rkt 14:2 (* x (ann 3 Integer)) -- exact real arith

END
""

#lang typed/racket
(provide f)

(: f : Float -> Real)
(define (f x)
  (* x (ann 3 Integer)))
