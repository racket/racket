#;#;
#<<END
TR info: multi-file1.rkt 5:2 (* x (ann 3 Integer)) -- possible exact real arith
TR missed opt: multi-file1.rkt 5:2 (* x (ann 3 Integer)) -- all args float-arg-expr, result not Float -- caused by: 5:12 3
END
""
#lang typed/racket
#reader tests/typed-racket/optimizer/reset-port
(provide f)

(: f : Float -> Real)
(define (f x)
  (* x (ann 3 Integer)))
