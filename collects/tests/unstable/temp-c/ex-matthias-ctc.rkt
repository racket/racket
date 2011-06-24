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
 (require 'a)

 (define memory (new memory%))

 (define a (send memory malloc))
 (send memory free a) (displayln `(freeing ,a))
 (send memory free "foo") (displayln `(freeing ,a))
 (send memory free a) (displayln `(freeing ,a)))

(require 'b)