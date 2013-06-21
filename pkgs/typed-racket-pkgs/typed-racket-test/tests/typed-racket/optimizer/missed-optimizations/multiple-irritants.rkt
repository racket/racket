#;#;
#<<END
TR info: multiple-irritants.rkt 13:0 (* (ann 4 Integer) (ann 5 Integer) 6.0) -- exact real arith
TR missed opt: multiple-irritants.rkt 13:0 (* (ann 4 Integer) (ann 5 Integer) 6.0) -- all args float-arg-expr, result not Float -- caused by: 13:8 4, 13:24 5
END
#<<END
120.0

END

#lang typed/racket

(* (ann 4 Integer) (ann 5 Integer) 6.0)
