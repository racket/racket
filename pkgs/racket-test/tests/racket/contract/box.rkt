#lang racket/base
(require "test-util.rkt")
(parameterize ([current-contract-namespace (make-basic-contract-namespace)])
  (test/no-error
   '(let ([v (chaperone-box (box-immutable 1)
                            (λ (box v) v)
                            (λ (box v) v))])
      (contract (box/c any/c) v 'pos 'neg)))

  (test/no-error
   '(let ([v (chaperone-box (box-immutable 1)
                            (λ (box v) v)
                            (λ (box v) v))])
      (contract (box/c none/c any/c) v 'pos 'neg)))

  (test/no-error
   '(let ([v (chaperone-box (box 1)
                            (λ (box v) v)
                            (λ (box v) v))])
      (set-box! (contract (box/c boolean? none/c) v 'pos 'neg) #t)))
  
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
   'box/c6
   '(set-box! (contract (box/c boolean? any/c) (box #f) 'pos 'neg) 11))
  
  (test/neg-blame
   'box/c7
   '(set-box! (contract (box/c boolean?) (box 12) 'pos 'neg) 11))

  (test/neg-blame
   'box/c7
   '(set-box! (contract (box/c boolean? any/c) (box 12) 'pos 'neg) 11))
  
  (test/pos-blame
   'box/c8
   '(contract ((values box/c) any/c) #f 'pos 'neg))

  (test/pos-blame
   'box/c9
   '(unbox (contract (box/c real? (>/c 0)) (box -1) 'pos 'neg)))

  (test/neg-blame
   'box/c10
   '(set-box! (contract (box/c (>/c 0) real?) (box -1) 'pos 'neg) -2))

  (test/pos-blame
   'box/c11
   '(unbox (contract (box/c integer?) (box 1.1) 'pos 'neg)))

  (test/neg-blame
   'box/c12
   '(set-box! (contract (box/c integer?) (box 1) 'pos 'neg) 1.5))

  (test/pos-blame
   'box/c13
   '(let ()
      (define N 18)

      (define c
        (for/fold ([c (-> integer? integer?)])
                  ([i (in-range N)])
          (box/c c)))

      (define val
        (for/fold ([val 5])
                  ([i (in-range N)])
          (box val)))

      (define cval (contract c val 'pos 'neg))

      (for/fold ([val cval])
                ([i (in-range N)])
        (unbox val))))
  
  ;; contract-stronger? tests
  (contract-eval '(require (only-in racket/contract/combinator contract-stronger?)))

  (test/spec-passed/result
   'box/c-stronger1
   '(contract-stronger? (box/c integer?) (box/c integer?))
   #t)

  (test/spec-passed/result
   'box/c-stronger2
   '(contract-stronger? (box/c (>/c 0)) (box/c real?))
   #f)

  (test/spec-passed/result
   'box/c-stronger3
   '(contract-stronger? (box/c real?) (box/c (>/c 0)))
   #f)

  (test/spec-passed/result
   'box/c-stronger4
   '(contract-stronger? (box/c integer? any/c) (box/c integer? any/c))
   #t)

  (test/spec-passed/result
   'box/c-stronger5
   '(contract-stronger? (box/c any/c integer?) (box/c any/c integer?))
   #t)

  (test/spec-passed/result
   'box/c-stronger6
   '(contract-stronger? (box/c real? any/c) (box/c (>/c 0) any/c))
   #t)

  (test/spec-passed/result
   'box/c-stronger7
   '(contract-stronger? (box/c any/c real?) (box/c any/c (>/c 0)))
   #f)

  (test/spec-passed/result
   'box/c-stronger8
   '(contract-stronger? (box/c (>/c 0) any/c) (box/c real? any/c))
   #f)

  (test/spec-passed/result
   'box/c-stronger9
   '(contract-stronger? (box/c any/c (>/c 0)) (box/c any/c real?))
   #t)

  (test/spec-passed/result
   'box/c-stronger10
   '(contract-stronger? (box/c real? (>/c 0)) (box/c (>/c 0) real?))
   #t)
  
  (test/neg-blame
   'box/c-with-cons/c-inside
   '(let ([f
           (contract (box/c (cons/c (-> boolean? boolean?) '()))
                     (box (list values))
                     'pos 
                     'neg)])
      ((car (unbox f)) 3)))

  (test/neg-blame
   'box/c-with-cons/c-inside-r
   '(let ([f
           (contract (box/c any/c (cons/c (-> boolean? boolean?) '()))
                     (box (list values))
                     'pos
                     'neg)])
      ((car (unbox f)) 3)))

  (test/no-error
   '(let ([f
           (contract (box/c (cons/c (-> boolean? boolean?) '()) any/c)
                     (box (list values))
                     'pos 
                     'neg)])
      ((car (unbox f)) 3))))
