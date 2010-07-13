#lang typed/scheme 
(require racket/unsafe/ops)
(if (number? "eh")
    (+ 2.0 3.0)
    (+ 4.0 5.0))
(if #f
    (+ 2.0 3.0)
    (+ 4.0 5.0))
