#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 'racket/set)])

  (test/spec-passed/result
   'set/c1
   '(contract (set/c integer?)
              (set 0)
              'pos 'neg)
   (contract-eval '(set 0)))

  (test/pos-blame
   'set/c2
   '(contract (set/c integer?)
              (set #t)
              'pos 'neg))

  (test/pos-blame
   'set/c3
   '(contract (set/c integer? #:cmp 'eq)
              (set 0)
              'pos 'neg))

  (test/pos-blame
   'set/c4
   '(contract (set/c integer? #:cmp 'eqv)
              (set 0)
              'pos 'neg))

  (test/pos-blame
   'set/c5
   '(contract (set/c integer? #:cmp 'equal)
              (seteq 0)
              'pos 'neg))

  (test/spec-passed/result
   'set/c6
   '(set-map (contract (set/c integer?)
                       (set 0)
                       'pos 'neg)
             values)
   (list 0))

  (test/neg-blame
   'set/c7
   '(let ([s (set-map (contract (set/c (-> integer? integer?))
                                (set (λ (x) #f))
                                'pos 'neg)
                      values)])
      ((car s) #f)))

  (test/pos-blame
   'set/c8
   '(let ([s (set-map (contract (set/c (-> integer? integer?))
                                (set (λ (x) #f))
                                'pos 'neg)
                      values)])
      ((car s) 1)))

  (test/pos-blame
   'set/c9
   '(contract (set/c integer?)
              (list 0)
              'pos 'neg))

  (test/pos-blame
   'set/c10
   '(contract (set/c integer?)
              (mutable-set 0)
              'pos 'neg))

  (test/spec-passed/result
   'set/c11
   '(contract (set/c integer? #:kind 'dont-care)
              (list 0)
              'pos 'neg)
   (list 0))

  (test/spec-passed/result
   'set/c12
   '(contract (set/c integer? #:kind 'dont-care)
              (set 0)
              'pos 'neg)
   (contract-eval '(set 0)))

  (test/spec-passed/result
   'set/c13
   '(contract (set/c integer? #:kind 'dont-care)
              (mutable-set 0)
              'pos 'neg)
   (contract-eval '(mutable-set 0)))

  (test/pos-blame
   'set/c14
   '(contract (set/c integer? #:kind 'mutable)
              (list 0)
              'pos 'neg))

  (test/pos-blame
   'set/c15
   '(contract (set/c integer? #:kind 'mutable)
              (set 0)
              'pos 'neg))

  (test/spec-passed/result
   'set/c16
   '(contract (set/c integer? #:kind 'mutable)
              (mutable-set 0)
              'pos 'neg)
   (contract-eval '(mutable-set 0)))


  (test/pos-blame
   'set/c17
   '(let ()
      (struct binary-set [integer]
        #:transparent
        #:methods gen:set
        [(define (set-member? st i)
           (bitwise-bit-set? (binary-set-integer st) i))
         (define (set-add st i)
           (binary-set (bitwise-ior (binary-set-integer st)
                                    (arithmetic-shift 1 i))))
         (define (set-remove st i)
           (binary-set (bitwise-and (binary-set-integer st)
                                    (bitwise-not (arithmetic-shift 1 i)))))])
      (contract (set/c integer?)
                (binary-set 5)
                'pos 'neg)))

  (test/spec-passed
   'set/c19
   '(let ()
      (struct binary-set [integer]
        #:transparent
        #:methods gen:set
        [(define (set-member? st i)
           (bitwise-bit-set? (binary-set-integer st) i))
         (define (set-add st i)
           (binary-set (bitwise-ior (binary-set-integer st)
                                    (arithmetic-shift 1 i))))
         (define (set-remove st i)
           (binary-set (bitwise-and (binary-set-integer st)
                                    (bitwise-not (arithmetic-shift 1 i)))))])
      (contract (set/c integer? #:kind 'dont-care)
                (binary-set 5)
                'pos 'neg)))

  (test/spec-passed
   'set/c20
   '(let ()
      (struct binary-set [integer]
        #:transparent
        #:methods gen:set
        [(define (set-member? st i)
           (bitwise-bit-set? (binary-set-integer st) i))
         (define (set-add st i)
           (binary-set (bitwise-ior (binary-set-integer st)
                                    (arithmetic-shift 1 i))))
         (define (set-remove st i)
           (binary-set (bitwise-and (binary-set-integer st)
                                    (bitwise-not (arithmetic-shift 1 i)))))])
      (contract (set/c boolean? #:kind 'dont-care #:lazy? #t)
                (binary-set 5)
                'pos 'neg)))

  (test/spec-passed/result
   'set/c21
   '(let* ([c (set/c (-> integer? integer?))]
           [s (contract c (set (λ (x) x)) 'pos 'neg)])
      (and (has-contract? s)
           (equal? (value-contract s) c)))
   #t)

  (test/spec-passed/result
   'set/c2b
   '(let* ([c (set/c (-> integer? integer?))]
           [s (contract c (set (λ (x) x)) 'pos 'neg)])
      (has-blame? s))
   #t)

  (test/spec-passed
   'set/c22
   '(contract (set/c (-> integer? integer?) #:lazy? #t)
              (set #f) 'pos 'neg))

  (test/pos-blame
   'set/c23
   '(set-first
     (contract (set/c (-> integer? integer?) #:lazy? #t)
               (set #f) 'pos 'neg)))

  (test/pos-blame
   'set/c24
   '(contract (set/c (-> integer? integer?) #:lazy? #f)
              (set #f) 'pos 'neg))

  (test/spec-passed
   'set/c25
   '(contract (set/c integer? #:lazy? #t)
              (set #f) 'pos 'neg))

  (test/pos-blame
   'set/c26
   '(set-first
     (contract (set/c integer? #:lazy? #t)
               (set #f) 'pos 'neg)))

  (test/pos-blame
   'set/c27
   '(contract (set/c integer? #:lazy? #f)
              (set #f) 'pos 'neg))

  (test/neg-blame
   'set/c28
   '(let ([s (contract (set/c integer? #:lazy? #t #:kind 'dont-care)
                       (mutable-set #f) 'pos 'neg)])
      (set-add! s "x")))

  (test/neg-blame
   'set/c29
   '(let ([s (contract (set/c integer? #:lazy? #f #:kind 'mutable)
                       (mutable-set 0) 'pos 'neg)])
      (set-add! s "x")))

  (test/spec-passed
   'set/c30
   '(let ()
      (define-custom-set-types set2 equal?)
      (set-add
       (contract (set/c (-> integer? integer?))
                 (make-immutable-set2)
                 'pos 'neg)
       add1)))

  (test/spec-passed
   'set/c30
   '(let ()
      (define-custom-set-types set2 equal?)
      (set-add
       (contract (set/c (-> integer? integer?))
                 (make-immutable-set2)
                 'pos 'neg)
       add1)))
  
  )
