#lang typed/scheme

(: foo : Any -> (U 'x 'y))
(define (foo x)
  (ann
   (if (member x '(x y))
       x
       'x)
   (U 'x 'y))

  (ann
   (if (memv x '(x y))
       x
       'x)
   (U 'x 'y))

  (if (memq x '(x y))
      x
      'x))
