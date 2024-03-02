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
   'treelist/c8
   '(contract (treelist/c (-> integer? integer?) #:lazy? #t)
              (treelist (λ (x) #f))
              'pos
              'neg))
  (test/pos-blame
   'treelist/c9
   '((treelist-first
      (contract (treelist/c (-> integer? integer?) #:lazy? #t)
                (treelist (λ (x) #f))
                'pos
                'neg))
     1))
  (test/pos-blame
   'treelist/c10
   '(treelist-ref
     (treelist-append
      (contract (treelist/c number? #:lazy? #t #:flat? #f)
                (treelist "one")
                'pos 'neg)
      (contract (treelist/c number? #:lazy? #t #:flat? #f)
                (treelist #t)
                'something 'else))
     0))
  (test/pos-blame
   'treelist/c11
   '(treelist-ref
     (treelist-append
      (contract (treelist/c number? #:lazy? #t #:flat? #f)
                (treelist "one")
                'something 'else)
      (contract (treelist/c number? #:lazy? #t #:flat? #f)
                (treelist #t)
                'pos 'neg))
     1))

  ;; tests that make sure that
  ;; the contract does not get put
  ;; onto newly added things in the treelist
  (test/spec-passed/result
   'treelist/c-add.set
   '(treelist-ref
     (treelist-set
      (contract
       (treelist/c number? #:lazy? #t #:flat? #f)
       (treelist 1 2 3)
       'pos
       'neg)
      0
      "abc")
     0)
   "abc")
  (test/spec-passed/result
   'treelist/c-add.insert
   '(treelist-ref
     (treelist-insert
      (contract
       (treelist/c number? #:lazy? #t #:flat? #f)
       (treelist 1 2 3)
       'pos
       'neg)
      2
      "abc")
     2)
   "abc")
  (test/spec-passed/result
   'treelist/c-add.append
   '(let ([t
           (treelist-append
            (contract
             (treelist/c number? #:lazy? #t #:flat? #f)
             (treelist 1 2 3)
             'pos
             'neg)
            (treelist "abc" "def" "ghi"))])
      (list (treelist-ref t 3)
            (treelist-ref t 4)
            (treelist-ref t 5)))
   (list "abc" "def" "ghi"))
  (test/spec-passed/result
   'treelist/c-add.prepend
   '(let ([t
           (treelist-append
            (treelist "abc" "def" "ghi")
            (contract
             (treelist/c number? #:lazy? #t #:flat? #f)
             (treelist 1 2 3)
             'pos
             'neg))])
      (list (treelist-ref t 0)
            (treelist-ref t 1)
            (treelist-ref t 2)))
   (list "abc" "def" "ghi"))
  (test/spec-passed/result
   'treelist/c-add.delete
   '(let ([t
           (treelist-append
            (treelist-delete
             (contract
              (treelist/c number? #:lazy? #t #:flat? #f)
              (treelist 1 2 3)
              'pos
              'neg)
             0)
            (treelist "abc"))])
      (treelist-ref t 2))
   "abc")
  (test/spec-passed/result
   'treelist/c-add.take
   '(let ([t
           (treelist-append
            (treelist-take
             (contract
              (treelist/c number? #:lazy? #t #:flat? #f)
              (treelist 1 2 3)
              'pos
              'neg)
             2)
            (treelist "abc"))])
      (treelist-ref t 2))
   "abc")
  (test/spec-passed/result
   'treelist/c-add.drop
   '(let ([t
           (treelist-append
            (treelist-drop
             (contract
              (treelist/c number? #:lazy? #t #:flat? #f)
              (treelist 1 2 3)
              'pos
              'neg)
             1)
            (treelist "abc"))])
      (treelist-ref t 2))
   "abc")
  )
