#;#;
#<<END
TR info: nested-same-kind.rkt 5:0 (* 2.0 (* 3.0 (ann 4 Integer))) -- possible exact real arith
TR info: nested-same-kind.rkt 5:7 (* 3.0 (ann 4 Integer)) -- possible exact real arith
TR info: nested-same-kind.rkt 6:0 (* 1.0 (* 2.0 (* 3.0 (ann 4 Integer)))) -- possible exact real arith
TR info: nested-same-kind.rkt 6:14 (* 3.0 (ann 4 Integer)) -- possible exact real arith
TR info: nested-same-kind.rkt 6:7 (* 2.0 (* 3.0 (ann 4 Integer))) -- possible exact real arith
TR info: nested-same-kind.rkt 7:0 (* 2.0 (* 3.0 (ann 4 Integer) (ann 5 Integer))) -- possible exact real arith
TR info: nested-same-kind.rkt 7:7 (* 3.0 (ann 4 Integer) (ann 5 Integer)) -- possible exact real arith
TR info: nested-same-kind.rkt 8:0 (* (* 3.0 (ann 4 Integer)) (* 3.0 (ann 4 Integer))) -- possible exact real arith
TR info: nested-same-kind.rkt 8:27 (* 3.0 (ann 4 Integer)) -- possible exact real arith
TR info: nested-same-kind.rkt 8:3 (* 3.0 (ann 4 Integer)) -- possible exact real arith
TR missed opt: nested-same-kind.rkt 5:0 (* 2.0 (* 3.0 (ann 4 Integer))) -- all args float-arg-expr, result not Float -- caused by: 5:7 (* 3.0 (ann 4 Integer))
TR missed opt: nested-same-kind.rkt 5:7 (* 3.0 (ann 4 Integer)) -- all args float-arg-expr, result not Float -- caused by: 5:19 4
TR missed opt: nested-same-kind.rkt 6:0 (* 1.0 (* 2.0 (* 3.0 (ann 4 Integer)))) -- all args float-arg-expr, result not Float -- caused by: 6:7 (* 2.0 (* 3.0 (ann 4 Integer)))
TR missed opt: nested-same-kind.rkt 6:14 (* 3.0 (ann 4 Integer)) -- all args float-arg-expr, result not Float -- caused by: 6:26 4
TR missed opt: nested-same-kind.rkt 6:7 (* 2.0 (* 3.0 (ann 4 Integer))) -- all args float-arg-expr, result not Float -- caused by: 6:14 (* 3.0 (ann 4 Integer))
TR missed opt: nested-same-kind.rkt 7:0 (* 2.0 (* 3.0 (ann 4 Integer) (ann 5 Integer))) -- all args float-arg-expr, result not Float -- caused by: 7:7 (* 3.0 (ann 4 Integer) (ann 5 Integer))
TR missed opt: nested-same-kind.rkt 7:7 (* 3.0 (ann 4 Integer) (ann 5 Integer)) -- all args float-arg-expr, result not Float -- caused by: 7:19 4, 7:35 5
TR missed opt: nested-same-kind.rkt 8:0 (* (* 3.0 (ann 4 Integer)) (* 3.0 (ann 4 Integer))) -- all args float-arg-expr, result not Float -- caused by: 8:3 (* 3.0 (ann 4 Integer)), 8:27 (* 3.0 (ann 4 Integer))
TR missed opt: nested-same-kind.rkt 8:27 (* 3.0 (ann 4 Integer)) -- all args float-arg-expr, result not Float -- caused by: 8:39 4
TR missed opt: nested-same-kind.rkt 8:3 (* 3.0 (ann 4 Integer)) -- all args float-arg-expr, result not Float -- caused by: 8:15 4
END
#<<END
24.0
24.0
120.0
144.0

END
#lang typed/racket
#reader tests/typed-racket/optimizer/reset-port
;; when a single "bubble" causes missed optimizations to cascade, a single
;; close call should be reported, with the outermost expression being reported
;; but with the innermost flagged as the cause

(* 2.0 (* 3.0 (ann 4 Integer)))
(* 1.0 (* 2.0 (* 3.0 (ann 4 Integer))))
(* 2.0 (* 3.0 (ann 4 Integer) (ann 5 Integer)))
(* (* 3.0 (ann 4 Integer)) (* 3.0 (ann 4 Integer)))
