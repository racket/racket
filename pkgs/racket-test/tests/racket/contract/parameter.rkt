#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 'racket/contract/parametric)])
  
  (test/neg-blame
   'parameter/c1
   '((contract (parameter/c integer?)
               (make-parameter 1)
               'pos 'neg)
     #f))
  
  (test/pos-blame
   'parameter/c2
   '((contract (parameter/c integer?)
               (make-parameter 'not-an-int)
               'pos 'neg)))
  
  (test/pos-blame
   'parameter/c3
   '((contract (parameter/c integer? string?)
               (make-parameter 'not-an-int number->string)
               'pos 'neg)))
  
  (test/neg-blame
   'parameter/c4
   '((contract (parameter/c integer? string?)
               (make-parameter 5 number->string)
               'pos 'neg)
     'not-an-int))
  
  (test/spec-passed
   'parameter/c5
   '((contract (parameter/c integer? string?)
               (make-parameter "foo" number->string)
               'pos 'neg)))
  
  (test/spec-passed
   'parameter/c6
   '((contract (parameter/c integer? string?)
               (make-parameter "foo" number->string)
               'pos 'neg)
     5))
  
  (test/pos-blame
   'parameter/c7
   '((contract (parameter/c integer? string?)
               (make-parameter 5 values)
               'pos 'neg)))

  (test/spec-passed/result
   'parameter/c8
   '(let ([p (make-parameter 1)])
      (parameter-procedure=?
       (contract (parameter/c integer?)
                 p 'pos 'neg)
       p))
   #t)

  (test/spec-passed/result
   'parameter/c9
   '(let ([p (make-parameter 1)])
      (chaperone-of?
       (contract (parameter/c integer?)
                 p 'pos 'neg)
       p))
   #f)

  (test/spec-passed/result
   'parameter/c9b
   '(let ([p (make-parameter (λ (x) x))])
      (chaperone-of?
       (contract (parameter/c (-> integer? integer?)
                              #:impersonator? #f)
                 p 'pos 'neg)
       p))
   #t)
  
  (test/spec-passed/result
   'parameter/c10
   '(let ([p (make-parameter 1)])
      (parameter?
       (contract (parameter/c integer?)
                 p 'pos 'neg)))
   #t)


  (test/spec-passed/result
   'parameter/c11
   '(chaperone-contract?
     (parameter/c integer?))
   #f)

  (test/spec-passed/result
   'parameter/c11b
   '(chaperone-contract?
     (parameter/c integer?
                  #:impersonator? #f))
   #t)

  (test/spec-passed/result
   'parameter/c12
   '(chaperone-contract?
     (parameter/c (-> integer? integer?)))
   #f)

  (test/spec-passed/result
   'parameter/c12b
   '(chaperone-contract?
     (parameter/c (-> integer? integer?)
                  #:impersonator? #f))
   #t)

  (test/spec-passed/result
   'parameter/c13
   '(chaperone-contract?
     (parameter/c (new-∀/c 'α)))
   #f)


  (test/spec-passed/result
   'parameter/c14
   '(chaperone-contract?
     (parameter/c (new-∀/c 'α)
                  integer?))
   #f)

  (test/spec-passed/result
   'parameter/c15
   '(chaperone-contract?
     (parameter/c integer?
                  (new-∀/c 'α)))
   #f)

  (test/spec-passed/result
   'parameter/c16
   '(chaperone-contract?
     (parameter/c integer?
                  (-> integer? integer?)
                  #:impersonator? #f))
   #t)

  (test/spec-passed/result
   'parameter/c16b
   '(chaperone-contract?
     (parameter/c integer?
                  (-> integer? integer?)))
   #f))
