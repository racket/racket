#;
(
TR missed opt: nested-same-kind.rkt 34:7 (* 3.0 (ann 4 Integer)) -- all args float-arg-expr, result not Float -- caused by: 34:19 4
TR info: nested-same-kind.rkt 34:7 (* 3.0 (ann 4 Integer)) -- exact real arith
TR missed opt: nested-same-kind.rkt 34:0 (* 2.0 (* 3.0 (ann 4 Integer))) -- all args float-arg-expr, result not Float -- caused by: 34:7 (* 3.0 (ann 4 Integer))
TR info: nested-same-kind.rkt 34:0 (* 2.0 (* 3.0 (ann 4 Integer))) -- exact real arith
TR missed opt: nested-same-kind.rkt 35:14 (* 3.0 (ann 4 Integer)) -- all args float-arg-expr, result not Float -- caused by: 35:26 4
TR info: nested-same-kind.rkt 35:14 (* 3.0 (ann 4 Integer)) -- exact real arith
TR missed opt: nested-same-kind.rkt 35:7 (* 2.0 (* 3.0 (ann 4 Integer))) -- all args float-arg-expr, result not Float -- caused by: 35:14 (* 3.0 (ann 4 Integer))
TR info: nested-same-kind.rkt 35:7 (* 2.0 (* 3.0 (ann 4 Integer))) -- exact real arith
TR missed opt: nested-same-kind.rkt 35:0 (* 1.0 (* 2.0 (* 3.0 (ann 4 Integer)))) -- all args float-arg-expr, result not Float -- caused by: 35:7 (* 2.0 (* 3.0 (ann 4 Integer)))
TR info: nested-same-kind.rkt 35:0 (* 1.0 (* 2.0 (* 3.0 (ann 4 Integer)))) -- exact real arith
TR missed opt: nested-same-kind.rkt 36:7 (* 3.0 (ann 4 Integer) (ann 5 Integer)) -- all args float-arg-expr, result not Float -- caused by: 36:19 4, 36:35 5
TR info: nested-same-kind.rkt 36:7 (* 3.0 (ann 4 Integer) (ann 5 Integer)) -- exact real arith
TR missed opt: nested-same-kind.rkt 36:0 (* 2.0 (* 3.0 (ann 4 Integer) (ann 5 Integer))) -- all args float-arg-expr, result not Float -- caused by: 36:7 (* 3.0 (ann 4 Integer) (ann 5 Integer))
TR info: nested-same-kind.rkt 36:0 (* 2.0 (* 3.0 (ann 4 Integer) (ann 5 Integer))) -- exact real arith
TR missed opt: nested-same-kind.rkt 37:3 (* 3.0 (ann 4 Integer)) -- all args float-arg-expr, result not Float -- caused by: 37:15 4
TR info: nested-same-kind.rkt 37:3 (* 3.0 (ann 4 Integer)) -- exact real arith
TR missed opt: nested-same-kind.rkt 37:27 (* 3.0 (ann 4 Integer)) -- all args float-arg-expr, result not Float -- caused by: 37:39 4
TR info: nested-same-kind.rkt 37:27 (* 3.0 (ann 4 Integer)) -- exact real arith
TR missed opt: nested-same-kind.rkt 37:0 (* (* 3.0 (ann 4 Integer)) (* 3.0 (ann 4 Integer))) -- all args float-arg-expr, result not Float -- caused by: 37:3 (* 3.0 (ann 4 Integer)), 37:27 (* 3.0 (ann 4 Integer))
TR info: nested-same-kind.rkt 37:0 (* (* 3.0 (ann 4 Integer)) (* 3.0 (ann 4 Integer))) -- exact real arith
24.0
24.0
120.0
144.0
)

#lang typed/racket
;; when a single "bubble" causes missed optimizations to cascade, a single
;; close call should be reported, with the outermost expression being reported
;; but with the innermost flagged as the cause

(* 2.0 (* 3.0 (ann 4 Integer)))
(* 1.0 (* 2.0 (* 3.0 (ann 4 Integer))))
(* 2.0 (* 3.0 (ann 4 Integer) (ann 5 Integer)))
(* (* 3.0 (ann 4 Integer)) (* 3.0 (ann 4 Integer)))
