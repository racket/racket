#lang typed/scheme

(: foo : Any -> (U 'x 'y))

(define (foo x)
  (if (member x '(x y))
      x
      'x))
