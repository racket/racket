#;#;
#<<END
TR missed opt: multiple-irritants.rkt 14:0 (* (ann 4 Integer) (ann 5 Integer) 6.0) -- all args float-arg-expr, result not Float -- caused by: 14:8 4, 14:24 5
TR info: multiple-irritants.rkt 14:0 (* (ann 4 Integer) (ann 5 Integer) 6.0) -- exact real arith

END
#<<END
120.0

END

#lang typed/racket

(* (ann 4 Integer) (ann 5 Integer) 6.0)
