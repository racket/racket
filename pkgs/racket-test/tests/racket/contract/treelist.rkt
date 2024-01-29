#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace
                 'racket/treelist
                 'racket/contract/parametric)])

  (test/spec-passed/result
   'treelist/c-which-kind.1
   '(flat-contract? (treelist/c boolean?))
   #t)
  (test/spec-passed/result
   'treelist/c-which-kind.2
   '(flat-contract? (treelist/c (-> integer? integer?)))
   #f)
  (test/spec-passed/result
   'treelist/c-which-kind.3
   '(chaperone-contract? (treelist/c (-> integer? integer?)))
   #t)
  (test/spec-passed/result
   'treelist/c-which-kind.4
   '(flat-contract? (treelist/c (new-∀/c 'α)))
   #f)
  (test/spec-passed/result
   'treelist/c-which-kind.5
   '(chaperone-contract? (treelist/c (new-∀/c 'α)))
   #f)
  (test/spec-passed/result
   'treelist/c-which-kind.6
   '(contract? (treelist/c (new-∀/c 'α)))
   #t)
  (test/spec-passed/result
   'treelist/c-which-kind.7
   '(flat-contract? (treelist/c boolean? #:flat? #f))
   #f)
  (test/spec-passed/result
   'treelist/c-which-kind.8
   '(chaperone-contract? (treelist/c boolean? #:flat? #f))
   #t)
  (test/spec-passed/result
   'treelist/c-which-kind.9
   '(flat-contract? (treelist/c boolean? #:lazy? #f #:flat? #f))
   #f)
  (test/spec-passed/result
   'treelist/c-which-kind.10
   '(chaperone-contract? (treelist/c boolean? #:lazy? #f #:flat? #f))
   #t)
  (test/spec-passed/result
   'treelist/c-which-kind.11
   '(flat-contract? (treelist/c boolean? #:lazy? #t #:flat? #f))
   #f)
  (test/spec-passed/result
   'treelist/c-which-kind.12
   '(chaperone-contract? (treelist/c boolean? #:lazy? #t #:flat? #f))
   #t)

  (test/spec-passed
   'treelist/c1
   '(contract (treelist/c boolean?)
              (treelist)
              'pos
              'neg))
  (test/spec-passed
   'treelist/c2
   '(contract (treelist/c boolean?)
              (treelist #t)
              'pos
              'neg))
  (test/pos-blame
   'treelist/c3
   '(contract (treelist/c boolean?)
              (treelist "true")
              'pos
              'neg))

  (test/spec-passed
   'treelist/c4
   '(contract (treelist/c boolean? #:lazy? #t #:flat? #f)
              (treelist)
              'pos
              'neg))
  (test/spec-passed
   'treelist/c5
   '(contract (treelist/c boolean? #:lazy? #t #:flat? #f)
              (treelist "true")
              'pos
              'neg))
  (test/pos-blame
   'treelist/c6
   '(treelist-first
     (contract (treelist/c boolean? #:lazy? #t #:flat? #f)
               (treelist "true")
               'pos
               'neg)))

  (test/spec-passed
   'treelist/c7
   '(contract (treelist/c (-> integer? integer?) #:lazy? #t)
              (treelist)
              'pos
              'neg))
  (test/spec-passed
   'treelist/c5
   '(contract (treelist/c (-> integer? integer?) #:lazy? #t)
              (treelist (λ (x) #f))
              'pos
              'neg))
  (test/pos-blame
   'treelist/c6
   '((treelist-first
      (contract (treelist/c (-> integer? integer?) #:lazy? #t)
                (treelist (λ (x) #f))
                'pos
                'neg))
     1)))
