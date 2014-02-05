#;#;
#<<END
TR info: multiple-irritants.rkt 2:0 (* (ann 4 Integer) (ann 5 Integer) 6.0) -- possible exact real arith
TR missed opt: multiple-irritants.rkt 2:0 (* (ann 4 Integer) (ann 5 Integer) 6.0) -- all args float-arg-expr, result not Float -- caused by: 2:8 4, 2:24 5
END
#<<END
120.0

END
#lang typed/racket
#reader tests/typed-racket/optimizer/reset-port

(* (ann 4 Integer) (ann 5 Integer) 6.0)
