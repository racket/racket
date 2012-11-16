#lang racket/load

(module mutator typed/racket
   (define: y : Integer 0)

   (: set-y! (Integer -> Void))
   (define (set-y! v)
     (set! y v))

   (provide y set-y!))

(module user typed/racket
   (require 'mutator)

   (: foo (Zero -> Zero))
   (define (foo y)
     (printf "(foo ~v)~n" y)
     y)

   (cond [(zero? y) (set-y! 10)
                     (foo y)]
         [else y]))

(require 'user)

