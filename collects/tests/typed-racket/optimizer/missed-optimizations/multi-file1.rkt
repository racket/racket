#;
(
TR missed opt: multi-file1.rkt 12:2 (* x (ann 3 Integer)) -- all args float-arg-expr, result not Float -- caused by: 12:12 3
)

#lang typed/racket

(provide f)

(: f : Float -> Real)
(define (f x)
  (* x (ann 3 Integer)))
