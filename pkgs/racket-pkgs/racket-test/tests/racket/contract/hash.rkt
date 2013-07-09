#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace (make-basic-contract-namespace)])
  (test/spec-passed
   'hash/c1
   '(contract (hash/c symbol? boolean?)
              (make-hash)
              'pos
              'neg))
  
  (test/spec-passed
   'hash/c1b
   '(contract (hash/c symbol? boolean? #:flat? #t)
              (make-hash)
              'pos
              'neg))
  
  (test/spec-passed
   'hash/c1c
   '(let ([h (contract (hash/c symbol? boolean?)
                       (make-hash)
                       'pos
                       'neg)])
      (hash-set! h 'x #t)
      (hash-ref h 'x)))
  
  (test/neg-blame
   'hash/c1d
   '(let ([h (contract (hash/c symbol? boolean?)
                       (make-hash)
                       'pos
                       'neg)])
      (hash-set! h 3 #t)))
  
  (test/neg-blame
   'hash/c1e
   '(let ([h (contract (hash/c symbol? boolean?)
                       (make-hash)
                       'pos
                       'neg)])
      (hash-set! h 'x 3)))
  
  (test/neg-blame
   'hash/c1f
   '(let ([h (contract (hash/c symbol? boolean?)
                       (make-hash)
                       'pos
                       'neg)])
      (hash-ref h 3)))
  
  (test/spec-passed
   'hash/c2
   '(contract (hash/c symbol? boolean?)
              (let ([h (make-hash)])
                (hash-set! h 'x #t)
                h)
              'pos
              'neg))
  
  (test/pos-blame
   'hash/c3
   '(write (contract (hash/c symbol? boolean?)
                     (let ([h (make-hash)])
                       (hash-set! h 'x 'x)
                       h)
                     'pos
                     'neg)
           (open-output-string)))
  
  ;; no io, so failure undetected
  (test/spec-passed
   'hash/c3b
   '(contract (hash/c symbol? boolean?)
              (let ([h (make-hash)])
                (hash-set! h 'x 'x)
                h)
              'pos
              'neg))
  
  (test/pos-blame
   'hash/c4
   '(write (contract (hash/c symbol? boolean?)
                     (let ([h (make-hash)])
                       (hash-set! h #t #f)
                       h)
                     'pos
                     'neg)
           (open-output-string)))
  
  ;; no io, so failure undetected
  (test/spec-passed
   'hash/c4b
   '(contract (hash/c symbol? boolean?)
              (let ([h (make-hash)])
                (hash-set! h #t #f)
                h)
              'pos
              'neg))
  
  (test/pos-blame
   'hash/c5
   '(contract (hash/c symbol? boolean? #:immutable #t)
              (let ([h (make-hash)])
                (hash-set! h 'x #f)
                h)
              'pos
              'neg))
  
  (test/spec-passed
   'hash/c6
   '(contract (hash/c symbol? boolean? #:immutable #t)
              (make-immutable-hash '((x . #f)))
              'pos
              'neg))
  
  (test/spec-passed
   'hash/c7
   '(contract (hash/c symbol? boolean? #:immutable #f)
              (let ([h (make-hash)])
                (hash-set! h 'x #f)
                h)
              'pos
              'neg))
  
  (test/pos-blame
   'hash/c8
   '(contract (hash/c symbol? boolean? #:immutable #f)
              (make-immutable-hash '((x . #f)))
              'pos
              'neg))
  
  (test/spec-passed
   'hash/c9
   '(contract (hash/c symbol? boolean? #:immutable 'dont-care)
              (make-immutable-hash '((x . #f)))
              'pos
              'neg))
  
  (test/spec-passed
   'hash/c10
   '(contract (hash/c symbol? boolean? #:immutable 'dont-care)
              (let ([h (make-hash)])
                (hash-set! h 'x #f)
                h)
              'pos
              'neg))
  
  (test/spec-passed/result
   'hash/c11
   '(hash-ref (contract (hash/c symbol? number? #:immutable #t)
                        (make-immutable-hash '((x . 1)))
                        'pos
                        'neg)
              'x)
   1)
  
  (test/spec-passed/result
   'hash/c12
   '(hash-ref (contract (hash/c symbol? number?)
                        (let ([ht (make-hash)])
                          (hash-set! ht 'x 1)
                          ht)
                        'pos
                        'neg)
              'x)
   1)
  
  (test/pos-blame
   'hash/c13a
   '(contract (hash/c (hash/c number? number?) number?)
              (make-hasheq)
              'pos
              'neg))
  
  (test/pos-blame
   'hash/c13b
   '(contract (hash/c (hash/c number? number?) number?)
              (make-hasheq)
              'pos
              'neg))
  
  (test/neg-blame
   'hash/c13c
   '(let ([h (contract (hash/c (hash/c number? number?) number?)
                       (make-hash)
                       'pos
                       'neg)])
      (hash-set! h (make-hash '((2 . 3))) 2)
      (hash-set! h (make-hash '((3 . #t))) 3)
      (for ([(k v) (in-hash h)])
        (hash-ref k v))))
  
  
  
  (test/no-error
   '(let ([v (chaperone-hash (make-immutable-hash (list (cons 1 2)))
                             (λ (hash k) (values k (λ (h k v) v)))
                             (λ (hash k v) (values k v))
                             (λ (hash k) k)
                             (λ (hash k) k))])
      (contract (hash/c any/c any/c) v 'pos 'neg)))
  
  (test/no-error
   '(let ([v (chaperone-hash (make-immutable-hasheq (list (cons 1 2)))
                             (λ (hash k) (values k (λ (h k v) v)))
                             (λ (hash k v) (values k v))
                             (λ (hash k) k)
                             (λ (hash k) k))])
      (contract (hash/c any/c any/c) v 'pos 'neg)))
  
  (test/no-error
   '(let ([v (chaperone-hash (make-immutable-hasheqv (list (cons 1 2)))
                             (λ (hash k) (values k (λ (h k v) v)))
                             (λ (hash k v) (values k v))
                             (λ (hash k) k)
                             (λ (hash k) k))])
      (contract (hash/c any/c any/c) v 'pos 'neg))))