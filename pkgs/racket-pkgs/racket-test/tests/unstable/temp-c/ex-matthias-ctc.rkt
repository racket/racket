#lang racket/load
(module a racket  
  (define memory%
    (class object%
      (super-new)
      (define/public (malloc) 1)
      (define/public (free n) (void))))
  
  (provide/contract
   [memory%
    (class/c [malloc (->m number?)]
             [free (->m number? void)])]))

(module b racket
  (require 'a tests/eli-tester)
  
  (define memory (new memory%))
  
  (define a (send memory malloc))
  (test
   (send memory free a) 
   (send memory free "foo") =error> #rx"expected: number\\?"
   (send memory free a)))

(require 'b)
