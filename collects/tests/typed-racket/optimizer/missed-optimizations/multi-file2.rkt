#;
(
TR opt: multi-file2.rkt 12:10 (+ 3 5) -- fixnum bounded expr
TR opt: multi-file2.rkt 12:3 (* 3.4 (+ 3 5)) -- binary float
81.6
)

#lang typed/racket

(require "multi-file1.rkt")

(f (* 3.4 (+ 3 5)))
