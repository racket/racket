#;
(
TR missed opt: nested-same-kind.rkt 24:0 (* 2.0 (* 3.0 (ann 4 Integer))) -- exact ops inside float expr -- caused by: 24:7 (* 3.0 (ann 4 Integer))
TR missed opt: nested-same-kind.rkt 24:0 (* 2.0 (* 3.0 (ann 4 Integer))) -- all args float-arg-expr, result not Float -- caused by: 24:19 4 (2 times)
TR missed opt: nested-same-kind.rkt 25:0 (* 1.0 (* 2.0 (* 3.0 (ann 4 Integer)))) -- exact ops inside float expr -- caused by: 25:14 (* 3.0 (ann 4 Integer)) (2 times)
TR missed opt: nested-same-kind.rkt 25:0 (* 1.0 (* 2.0 (* 3.0 (ann 4 Integer)))) -- all args float-arg-expr, result not Float -- caused by: 25:26 4 (3 times)
TR missed opt: nested-same-kind.rkt 26:0 (* 2.0 (* 3.0 (ann 4 Integer) (ann 5 Integer))) -- exact ops inside float expr -- caused by: 26:7 (* 3.0 (ann 4 Integer) (ann 5 Integer))
TR missed opt: nested-same-kind.rkt 26:0 (* 2.0 (* 3.0 (ann 4 Integer) (ann 5 Integer))) -- all args float-arg-expr, result not Float -- caused by: 26:19 4, 26:35 5 (2 times)
TR missed opt: nested-same-kind.rkt 27:0 (* (* 3.0 (ann 4 Integer)) (* 3.0 (ann 4 Integer))) -- exact ops inside float expr -- caused by: 27:3 (* 3.0 (ann 4 Integer)), 27:27 (* 3.0 (ann 4 Integer))
TR missed opt: nested-same-kind.rkt 27:0 (* (* 3.0 (ann 4 Integer)) (* 3.0 (ann 4 Integer))) -- all args float-arg-expr, result not Float -- caused by: 27:15 4, 27:39 4 (3 times)
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
