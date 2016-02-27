#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace
                 'racket/contract/parametric)])

  (test/spec-passed/result
   'rename1
   '(contract-name
     (rename-contract (-> integer? integer?)
                      'another-name))
   'another-name)

  (test/spec-passed/result
   'rename2
   '(chaperone-contract?
     (rename-contract (-> integer? integer?)
                      'another-name))
   #t)

  (test/spec-passed/result
   'rename3
   '(contract-name
     (rename-contract integer? 'another-name))
   'another-name)
  
  (test/spec-passed/result
   'rename4
   '(flat-contract?
     (rename-contract integer? 'another-name))
   #t)

  (test/spec-passed/result
   'rename5
   '(contract-name
     (rename-contract integer? 'another-name))
   'another-name)
  
  (test/spec-passed/result
   'rename6
   '(flat-contract?
     (rename-contract (new-∀/c 'alpha) 'α))
   #f)

  (test/spec-passed/result
   'rename7
   '(chaperone-contract?
     (rename-contract (new-∀/c 'alpha) 'α))
   #f)

  (test/spec-passed/result
   'rename8
   '(contract?
     (rename-contract (new-∀/c 'alpha) 'α))
   #t)

  (test/pos-blame
   'rename9
   '((contract (rename-contract (-> integer? integer?) 'whatever)
               (λ (x) #f)
               'pos 'neg)
     1))

  (test/neg-blame
   'rename10
   '((contract (rename-contract (-> integer? integer?) 'whatever)
               (λ (x) x)
               'pos 'neg)
     #f)))
