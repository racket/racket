#;
(
TR missed opt: multi-file1.rkt 12:2 (* x 3) -- all args float-arg-expr, result not Float -- caused by: 12:7 3
)

#lang typed/racket

(provide f)

(: f : Float -> Real)
(define (f x)
  (* x 3))
