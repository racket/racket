#;
(
TR missed opt: multi-file1.rkt 12:2 (* x 3) -- all args float-arg-expr, result not Float -- caused by: 12:7 3
TR opt: multi-file2.rkt 13:3 (* 3.4 (+ 3 5)) -- binary float
TR opt: multi-file2.rkt 13:10 (+ 3 5) -- fixnum bounded expr
81.6
)

#lang typed/racket

(require "multi-file1.rkt")

(f (* 3.4 (+ 3 5)))
