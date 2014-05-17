#lang racket/base
(require "test-util.rkt")
(parameterize ([current-contract-namespace (make-basic-contract-namespace)])
  (test/no-error
   '(let ([v (chaperone-box (box-immutable 1)
                            (λ (box v) v)
                            (λ (box v) v))])
      (contract (box/c any/c) v 'pos 'neg)))
  
  (test/pos-blame
   'box/c1
   '(contract (box/c any/c) #f 'pos 'neg))
  
  (test/pos-blame
   'box/c2
   '(unbox (contract (box/c integer?) (box #f) 'pos 'neg)))
  
  (test/pos-blame
   'box/c3
   '(contract (box/c integer?) (box-immutable #f) 'pos 'neg))
  
  (test/neg-blame
   'box/c-with-cons/c-inside
   '(let ([f
           (contract (box/c (cons/c (-> boolean? boolean?) '()))
                     (box (list values))
                     'pos 
                     'neg)])
      ((car (unbox f)) 3))))
