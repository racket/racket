#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 'racket/unit 'racket/class
                                               'racket/contract 'racket/set)])
  
  (ctest #f value-contract #f)
  (ctest #f value-contract (λ (x) x))
  (ctest #f value-contract (unit (import) (export)))
  (ctest #f value-contract object%)
  
  (contract-eval
   `(let ([ctc (-> number? number?)])
      (,test ctc value-contract (contract ctc (λ (x) x) 'pos 'neg))))
  (contract-eval
   `(let ([ctc (->* (number?) (number?) number?)])
      (,test ctc value-contract (contract ctc (λ (x [y 3]) x) 'pos 'neg))))
  (contract-eval
   `(let ([ctc (->d ([x number?]) ([y number?]) [_ number?])])
      (,test ctc value-contract (contract ctc (λ (x [y 3]) x) 'pos 'neg))))
  (contract-eval
   `(let ([ctc (->i ([x number?]) ([y number?]) [_ number?])])
      (,test ctc value-contract (contract ctc (λ (x [y 3]) x) 'pos 'neg))))
  (contract-eval
   `(let ([ctc (unconstrained-domain-> number?)])
      (,test ctc value-contract (contract ctc (λ (x) 3) 'pos 'neg))))
  (contract-eval
   `(let ([ctc (case-> (-> number? number? number?) (-> number? number?))])
      (,test ctc value-contract (contract ctc (case-lambda [(x) 3] [(x y) (+ x y)]) 'pos 'neg))))
  
  (contract-eval
   `(let ([ctc (box/c number?)])
      (,test ctc value-contract (contract ctc (box 3) 'pos 'neg))))
  (contract-eval
   `(let ([ctc (hash/c number? number?)])
      (,test ctc value-contract (contract ctc (make-hash) 'pos 'neg))))
  (contract-eval
   `(let ([ctc (vectorof number?)])
      (,test ctc value-contract (contract ctc (vector 1 2 3) 'pos 'neg))))
  (contract-eval
   `(let ([ctc (vector/c number? number?)])
      (,test ctc value-contract (contract ctc (vector 4 5) 'pos 'neg))))
  
  (contract-eval
   `(let ([ctc (object-contract)])
      (,test ctc value-contract (contract ctc (new object%) 'pos 'neg))))
  
  (test/spec-passed/result
   'value-contract
   '(let ()
      (define c (-> integer? integer?))
      (define f (contract c (λ (x) x) 'pos 'neg))
      ;; opt/c version doesn't yet have blame, so
      ;; we require only that when there is blame, that the blame is right.
      (or (and (has-contract? f)
               (equal? c (value-contract f)))
          #t))
   #t)
  
  (test/spec-passed/result
   'value-blame.1
   '(let ()
      (define f
        (contract (-> integer? integer?) (λ (x) x) 'pos 'neg))
      ;; opt/c version doesn't yet have blame, so
      ;; we require only that when there is blame, that the blame is right.
      (blame-positive (value-blame f)))
   'pos)

  (test/spec-passed/result
   'value-blame.2
   '(let ()
      (define f
        (contract (-> integer? integer?) (λ (x) x) 'pos 'neg))
      (blame-negative (value-blame f)))
   'neg)

  (test/spec-passed/result
   'value-blame.3
   '(let ()
      (define f
        (contract (set/c (-> integer? integer?) #:kind 'mutable) (mutable-set) 'pos 'neg))
      (blame-positive (value-blame f)))
   'pos)

  (test/spec-passed/result
   'value-blame.4
   '(let ()
      (define f
        (contract (set/c (-> integer? integer?) #:kind 'mutable) (mutable-set) 'pos 'neg))
      (blame-negative (value-blame f)))
   'neg)

  (test/spec-passed/result
   'value-blame.5
   '(let ()
      (define f
        (contract (->i () any) (λ () 1) 'pos 'neg))
      (list (blame-negative (value-blame f))
            (blame-positive (value-blame f))))
   '(neg pos))

  (test/spec-passed/result
   'value-blame.6
   '(let ()
      (define f
        (contract (box/c (-> integer? integer?)) (box (λ (x) 1)) 'pos 'neg))
      (list (blame-negative (value-blame f))
            (blame-positive (value-blame f))))
   '(neg pos))

  (test/spec-passed/result
   'value-blame.7
   '(let ()
      (struct s (a b))
      (define an-s
        (contract (struct/c s integer? (-> integer? any)) (s 1 void) 'pos 'neg))
      (list (blame-negative (value-blame an-s))
            (blame-positive (value-blame an-s))))
   '(neg pos))
  )
