#;
(
TR missed opt: nested-same-kind.rkt 25:0 (* 2.0 (* 3.0 (ann 4 Integer))) -- exact ops inside float expr -- caused by: 25:7 (* 3.0 (ann 4 Integer))
TR missed opt: nested-same-kind.rkt 25:0 (* 2.0 (* 3.0 (ann 4 Integer))) -- all args float-arg-expr, result not Float -- caused by: 25:19 4 (2 times)
TR missed opt: nested-same-kind.rkt 26:0 (* 1.0 (* 2.0 (* 3.0 (ann 4 Integer)))) -- exact ops inside float expr -- caused by: 26:14 (* 3.0 (ann 4 Integer)) (2 times)
TR missed opt: nested-same-kind.rkt 26:0 (* 1.0 (* 2.0 (* 3.0 (ann 4 Integer)))) -- all args float-arg-expr, result not Float -- caused by: 26:26 4 (3 times)
TR missed opt: nested-same-kind.rkt 27:0 (* 2.0 (* 3.0 (ann 4 Integer) (ann 5 Integer))) -- exact ops inside float expr -- caused by: 27:7 (* 3.0 (ann 4 Integer) (ann 5 Integer))
TR missed opt: nested-same-kind.rkt 27:0 (* 2.0 (* 3.0 (ann 4 Integer) (ann 5 Integer))) -- all args float-arg-expr, result not Float -- caused by: 27:19 4, 27:35 5 (2 times)
TR missed opt: nested-same-kind.rkt 28:0 (* (* 3.0 (ann 4 Integer)) (* 3.0 (ann 4 Integer))) -- exact ops inside float expr -- caused by: 28:27 (* 3.0 (ann 4 Integer))
TR missed opt: nested-same-kind.rkt 28:0 (* (* 3.0 (ann 4 Integer)) (* 3.0 (ann 4 Integer))) -- exact ops inside float expr -- caused by: 28:3 (* 3.0 (ann 4 Integer))
TR missed opt: nested-same-kind.rkt 28:0 (* (* 3.0 (ann 4 Integer)) (* 3.0 (ann 4 Integer))) -- all args float-arg-expr, result not Float -- caused by: 28:15 4, 28:39 4 (3 times)
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
