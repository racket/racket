#;
(
TR missed opt: nested-same-kind.rkt 20:0 (* 2.0 (* 3.0 (ann 4 Integer))) -- all args float-arg-expr, result not Float -- caused by: 20:19 4 (2 times)
TR missed opt: nested-same-kind.rkt 21:0 (* 1.0 (* 2.0 (* 3.0 (ann 4 Integer)))) -- all args float-arg-expr, result not Float -- caused by: 21:26 4 (3 times)
TR missed opt: nested-same-kind.rkt 22:0 (* 2.0 (* 3.0 (ann 4 Integer) (ann 5 Integer))) -- all args float-arg-expr, result not Float -- caused by: 22:19 4, 22:35 5 (2 times)
TR missed opt: nested-same-kind.rkt 23:0 (* (* 3.0 (ann 4 Integer)) (* 3.0 (ann 4 Integer))) -- all args float-arg-expr, result not Float -- caused by: 23:15 4, 23:39 4 (3 times)
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
