#lang racket/load

(module A typed/racket

 (struct: Point ([x : Integer] [y : Integer]))

 (provide (all-defined-out)))

(module B typed/racket
 (require 'A)

 (: lift (Point -> Point))
 (define (lift p)
   (struct-copy Point p [x (add1 (Point-x p))]))
 (lift (Point 3 4)))

(require 'B)
