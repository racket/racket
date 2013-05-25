#lang racket
(require racklog tests/eli-tester)

(define %factorial
  (%rel (x y x1 y1)
        [(0 1) !]
        [(x y) (%< x 0) ! %fail]
        [(x y) (%is x1 (- x 1))
               (%factorial x1 y1)
               (%is y (* y1 x))]))

(test
 (%which ()
         (%factorial 0 1))
 => empty
 (%more)
 => #f
 
 (%which ()
         (%factorial -1 1))
 => #f
 (%which (x)
         (%factorial 3 x))
 => `((x . 6))
 (%more)
 => #f)
