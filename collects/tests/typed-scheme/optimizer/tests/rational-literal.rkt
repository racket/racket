#;
(
TR opt: rational-literal.rkt 8:0 (+ 3/4 1.2) -- binary float
1.95
)
#lang typed/racket #:optimize
;; rational literals should be promoted to floats at compile time
(+ 3/4 1.2)
