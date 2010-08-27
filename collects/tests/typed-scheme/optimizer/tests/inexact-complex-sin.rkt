#lang typed/scheme
#:optimize
(require racket/unsafe/ops)
((lambda: ((t : Integer))
          (+ (sin (* t 6.28)) 0.0+0.0i))
 1)
