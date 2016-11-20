#lang racket/base
(require "test-util.rkt")
(parameterize ([current-contract-namespace
                (make-basic-contract-namespace
                 'racket/contract/combinator)])
  
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
  
  
  (test/pos-blame
   'vectorof7
   '(contract (vectorof integer? #:immutable #t)
              (vector-immutable #f)
              'pos 'neg))
  
  (test/pos-blame
   'vectorof8
   '(contract (vectorof integer? #:immutable #t)
              11
              'pos 'neg))
  
  (test/pos-blame
   'vectorof9
   '(contract (vectorof integer? #:immutable #t)
              (vector 11)
              'pos 'neg))
  
  (test/spec-passed
   'vectorof10
   '(contract (vectorof integer? #:flat? #t)
              (vector 11)
              'pos 'neg))
  
  (test/pos-blame
   'vectorof10
   '(contract (vectorof integer? #:flat? #t)
              (vector #f)
              'pos 'neg))
  
  (test/pos-blame
   'vectorof11
   '(contract (vectorof integer? #:flat? #t)
              (vector-immutable #f)
              'pos 'neg))
  
  (test/spec-passed
   'vectorof12
   '(contract (vectorof integer? #:flat? #t)
              (vector-immutable 11)
              'pos 'neg))
  
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
                   0 #f)))
  
  (test/pos-blame
   'vector/c7
   '(contract (vector/c integer? #:immutable #t)
              (vector-immutable #f)
              'pos 'neg))

  (test/spec-passed/result
   'vector/immutable-flat
   '(let ([x (vector-immutable 1 2 3)])
      (eq? (contract (vectorof integer?) x 'pos 'neg)
           x))
   '#true)

  (test/spec-passed/result
   'vector/c-impersonator
   '(vector-ref (contract (vectorof (make-contract #:late-neg-projection (λ (b) (λ (x n) (+ x 1)))))
                          (vector 0)
                          'pos 'neg)
                0)
   1
   2)

  (test/spec-passed/result
   'vectorof-eager
   '(vector-ref (contract (vectorof integer? #:eager #f) (vector-immutable 0 "") 'pos 'neg) 0)
   0)

  (test/spec-passed/result
   'vectorof-eager-1
   '(vector-ref (contract (vectorof integer? #:eager 1) (vector-immutable 0 "") 'pos 'neg) 0)
   0)


  (test/pos-blame
   'vectorof-eager-1-fail
   '(vector-ref (contract (vectorof integer? #:eager 2) (vector-immutable 0 "") 'pos 'neg) 1))

  (test/pos-blame
   'vectorof-eager-fail
   '(contract (vectorof integer? #:eager 5) (vector-immutable 0 "") 'pos 'neg))

  )
