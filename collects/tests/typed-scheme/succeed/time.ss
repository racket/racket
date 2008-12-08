#lang typed-scheme



 (: foo : Number Number -> Number)
 (define (foo x y)
   (* x y))

 (: bar : Number -> Number)
 (define (bar c)
   (: loop : Number Number -> Number)
   (define (loop n acc)
     (if (< 0 n)
         (loop (- n 1) (+ (foo c n) acc))
         acc))
   (loop 10000000 0))
 (time (bar 0))
