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
  
  (test/pos-blame
   'box/c4
   '(contract (box/c integer? #:immutable #t) (box-immutable #f) 'pos 'neg))
  
  (test/spec-passed
   'box/c5
   '(contract (box/c boolean? #:immutable #t) (box-immutable #f) 'pos 'neg))
  
  (test/neg-blame
   'box/c6
   '(set-box! (contract (box/c boolean?) (box #f) 'pos 'neg) 11))
  
  (test/neg-blame
   'box/c7
   '(set-box! (contract (box/c boolean?) (box 12) 'pos 'neg) 11))
  
  
  (test/neg-blame
   'box/c-with-cons/c-inside
   '(let ([f
           (contract (box/c (cons/c (-> boolean? boolean?) '()))
                     (box (list values))
                     'pos 
                     'neg)])
      ((car (unbox f)) 3))))
