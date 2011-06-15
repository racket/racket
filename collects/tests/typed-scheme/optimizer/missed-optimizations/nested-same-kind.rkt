#;
(
TR missed opt: nested-same-kind.rkt 25:0 (#%app * (quote 2.0) (#%app * (quote 3.0) (quote 4))) -- binary, args all float-arg-expr, return type not Float -- caused by: 25:19 (quote 4)
TR missed opt: nested-same-kind.rkt 25:0 (#%app * (quote 2.0) (#%app * (quote 3.0) (quote 4))) -- exact arithmetic subexpression inside a float expression, extra precision discarded -- caused by: 25:7 (#%app * (quote 3.0) (quote 4))
TR missed opt: nested-same-kind.rkt 26:0 (#%app * (quote 1.0) (#%app * (quote 2.0) (#%app * (quote 3.0) (quote 4)))) -- binary, args all float-arg-expr, return type not Float -- caused by: 26:26 (quote 4)
TR missed opt: nested-same-kind.rkt 26:0 (#%app * (quote 1.0) (#%app * (quote 2.0) (#%app * (quote 3.0) (quote 4)))) -- exact arithmetic subexpression inside a float expression, extra precision discarded -- caused by: 26:14 (#%app * (quote 3.0) (quote 4))
TR missed opt: nested-same-kind.rkt 27:0 (#%app * (quote 2.0) (#%app * (quote 3.0) (quote 4) (quote 5))) -- binary, args all float-arg-expr, return type not Float -- caused by: 27:19 (quote 4), 27:35 (quote 5)
TR missed opt: nested-same-kind.rkt 27:0 (#%app * (quote 2.0) (#%app * (quote 3.0) (quote 4) (quote 5))) -- exact arithmetic subexpression inside a float expression, extra precision discarded -- caused by: 27:7 (#%app * (quote 3.0) (quote 4) (quote 5))
TR missed opt: nested-same-kind.rkt 28:0 (#%app * (#%app * (quote 3.0) (quote 4)) (#%app * (quote 3.0) (quote 4))) -- binary, args all float-arg-expr, return type not Float -- caused by: 28:15 (quote 4), 28:39 (quote 4)
TR missed opt: nested-same-kind.rkt 28:0 (#%app * (#%app * (quote 3.0) (quote 4)) (#%app * (quote 3.0) (quote 4))) -- exact arithmetic subexpression inside a float expression, extra precision discarded -- caused by: 28:27 (#%app * (quote 3.0) (quote 4))
TR missed opt: nested-same-kind.rkt 28:0 (#%app * (#%app * (quote 3.0) (quote 4)) (#%app * (quote 3.0) (quote 4))) -- exact arithmetic subexpression inside a float expression, extra precision discarded -- caused by: 28:3 (#%app * (quote 3.0) (quote 4))
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
