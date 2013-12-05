#lang racket/base
(require "test-util.rkt")
(parameterize ([current-contract-namespace
                (make-basic-contract-namespace)])
  
  (test/spec-passed
   'vectorof1
   '(let ([v (chaperone-vector (vector-immutable 1)
                               (λ (vec i v) v)
                               (λ (vec i v) v))])
      (contract (vectorof any/c) v 'pos 'neg)))
  
  (test/spec-passed
   'vectorof2
   '(let ([v (chaperone-vector (vector-immutable 1)
                               (λ (vec i v) v)
                               (λ (vec i v) v))])
      (contract (vectorof any/c) v 'pos 'neg)))
  
  (test/pos-blame
   'vectorof3
   '(let ([v (chaperone-vector (vector-immutable 1)
                               (λ (vec i v) v)
                               (λ (vec i v) v))])
      (vector-ref (contract (vectorof boolean?) v 'pos 'neg) 0)))
  
  (test/pos-blame
   'vectorof4
   '(let ([v (vector 1)])
      (vector-ref (contract (vectorof boolean?) v 'pos 'neg) 0)))
  
  (test/neg-blame
   'vectorof5
   '(let ([v (vector 1)])
      (vector-set! (contract (vectorof integer?) v 'pos 'neg)
                   0 #f)))
  
  (test/neg-blame
   'vectorof6
   '(let ([v (chaperone-vector (vector 1)
                               (λ (vec i v) v)
                               (λ (vec i v) v))])
      (vector-set! (contract (vectorof integer?) v 'pos 'neg)
                   0 #f)))
  
  
  (test/spec-passed
   'vector/c1
   '(let ([v (chaperone-vector (vector-immutable 1)
                               (λ (vec i v) v)
                               (λ (vec i v) v))])
      (contract (vector/c any/c) v 'pos 'neg)))
  
  (test/spec-passed
   'vector/c2
   '(let ([v (chaperone-vector (vector-immutable 1)
                               (λ (vec i v) v)
                               (λ (vec i v) v))])
      (contract (vector/c any/c) v 'pos 'neg)))
  
  (test/pos-blame
   'vector/c3
   '(let ([v (chaperone-vector (vector-immutable 1)
                               (λ (vec i v) v)
                               (λ (vec i v) v))])
      (vector-ref (contract (vector/c boolean?) v 'pos 'neg) 0)))
  
  (test/pos-blame
   'vector/c4
   '(let ([v (vector 1)])
      (vector-ref (contract (vector/c boolean?) v 'pos 'neg) 0)))
  
  (test/neg-blame
   'vector/c5
   '(let ([v (vector 1)])
      (vector-set! (contract (vector/c integer?) v 'pos 'neg)
                   0 #f)))
  
  (test/neg-blame
   'vector/c6
   '(let ([v (chaperone-vector (vector 1)
                               (λ (vec i v) v)
                               (λ (vec i v) v))])
      (vector-set! (contract (vector/c integer?) v 'pos 'neg)
                   0 #f))))
