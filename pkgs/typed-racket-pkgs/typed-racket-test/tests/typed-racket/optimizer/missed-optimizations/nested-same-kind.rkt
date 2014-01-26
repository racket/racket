#;#;
#<<END
TR info: nested-same-kind.rkt 39:0 (* 2.0 (* 3.0 (ann 4 Integer))) -- possible exact real arith
TR info: nested-same-kind.rkt 39:7 (* 3.0 (ann 4 Integer)) -- possible exact real arith
TR info: nested-same-kind.rkt 40:0 (* 1.0 (* 2.0 (* 3.0 (ann 4 Integer)))) -- possible exact real arith
TR info: nested-same-kind.rkt 40:14 (* 3.0 (ann 4 Integer)) -- possible exact real arith
TR info: nested-same-kind.rkt 40:7 (* 2.0 (* 3.0 (ann 4 Integer))) -- possible exact real arith
TR info: nested-same-kind.rkt 41:0 (* 2.0 (* 3.0 (ann 4 Integer) (ann 5 Integer))) -- possible exact real arith
TR info: nested-same-kind.rkt 41:7 (* 3.0 (ann 4 Integer) (ann 5 Integer)) -- possible exact real arith
TR info: nested-same-kind.rkt 41:7 (* 3.0 (ann 4 Integer) (ann 5 Integer)) -- possible exact real arith
TR info: nested-same-kind.rkt 42:0 (* (* 3.0 (ann 4 Integer)) (* 3.0 (ann 4 Integer))) -- possible exact real arith
TR info: nested-same-kind.rkt 42:27 (* 3.0 (ann 4 Integer)) -- possible exact real arith
TR info: nested-same-kind.rkt 42:3 (* 3.0 (ann 4 Integer)) -- possible exact real arith
TR missed opt: nested-same-kind.rkt 39:0 (* 2.0 (* 3.0 (ann 4 Integer))) -- all args float-arg-expr, result not Float -- caused by: 39:7 (* 3.0 (ann 4 Integer))
TR missed opt: nested-same-kind.rkt 39:7 (* 3.0 (ann 4 Integer)) -- all args float-arg-expr, result not Float -- caused by: 39:19 4
TR missed opt: nested-same-kind.rkt 40:0 (* 1.0 (* 2.0 (* 3.0 (ann 4 Integer)))) -- all args float-arg-expr, result not Float -- caused by: 40:7 (* 2.0 (* 3.0 (ann 4 Integer)))
TR missed opt: nested-same-kind.rkt 40:14 (* 3.0 (ann 4 Integer)) -- all args float-arg-expr, result not Float -- caused by: 40:26 4
TR missed opt: nested-same-kind.rkt 40:7 (* 2.0 (* 3.0 (ann 4 Integer))) -- all args float-arg-expr, result not Float -- caused by: 40:14 (* 3.0 (ann 4 Integer))
TR missed opt: nested-same-kind.rkt 41:0 (* 2.0 (* 3.0 (ann 4 Integer) (ann 5 Integer))) -- all args float-arg-expr, result not Float -- caused by: 41:7 (* 3.0 (ann 4 Integer) (ann 5 Integer))
TR missed opt: nested-same-kind.rkt 41:7 (* 3.0 (ann 4 Integer) (ann 5 Integer)) -- all args float-arg-expr, result not Float -- caused by: 41:19 4
TR missed opt: nested-same-kind.rkt 41:7 (* 3.0 (ann 4 Integer) (ann 5 Integer)) -- all args float-arg-expr, result not Float -- caused by: 41:7 (* 3.0 (ann 4 Integer) (ann 5 Integer)), 41:35 5
TR missed opt: nested-same-kind.rkt 42:0 (* (* 3.0 (ann 4 Integer)) (* 3.0 (ann 4 Integer))) -- all args float-arg-expr, result not Float -- caused by: 42:3 (* 3.0 (ann 4 Integer)), 42:27 (* 3.0 (ann 4 Integer))
TR missed opt: nested-same-kind.rkt 42:27 (* 3.0 (ann 4 Integer)) -- all args float-arg-expr, result not Float -- caused by: 42:39 4
TR missed opt: nested-same-kind.rkt 42:3 (* 3.0 (ann 4 Integer)) -- all args float-arg-expr, result not Float -- caused by: 42:15 4
END
#<<END
24.0
24.0
120.0
144.0

END

#lang typed/racket
;; when a single "bubble" causes missed optimizations to cascade, a single
;; close call should be reported, with the outermost expression being reported
;; but with the innermost flagged as the cause

(* 2.0 (* 3.0 (ann 4 Integer)))
(* 1.0 (* 2.0 (* 3.0 (ann 4 Integer))))
(* 2.0 (* 3.0 (ann 4 Integer) (ann 5 Integer)))
(* (* 3.0 (ann 4 Integer)) (* 3.0 (ann 4 Integer)))
