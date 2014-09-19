#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 'racket/unit 'racket/class 'racket/contract)])
  
  (ctest #f value-contract #f)
  (ctest #f value-contract (λ (x) x))
  (ctest #f value-contract (unit (import) (export)))
  (ctest #f value-contract object%)
  
  (contract-eval
   #:test-case-name 'value-contract1
   `(let ([ctc (-> number? number?)])
      (,test
       #:test-case-name 'value-contract1
       ctc value-contract (contract ctc (λ (x) x) 'pos 'neg))))
  (contract-eval
   #:test-case-name 'value-contract2
   `(let ([ctc (->* (number?) (number?) number?)])
      (,test
       #:test-case-name 'value-contract2
       ctc value-contract (contract ctc (λ (x [y 3]) x) 'pos 'neg))))
  (contract-eval
   #:test-case-name 'value-contract3
   `(let ([ctc (->d ([x number?]) ([y number?]) [_ number?])])
      (,test
       #:test-case-name 'value-contract3
       ctc value-contract (contract ctc (λ (x [y 3]) x) 'pos 'neg))))
  (contract-eval
   #:test-case-name 'value-contract4
   `(let ([ctc (->i ([x number?]) ([y number?]) [_ number?])])
      (,test
       #:test-case-name 'value-contract4
       ctc value-contract (contract ctc (λ (x [y 3]) x) 'pos 'neg))))
  (contract-eval
   #:test-case-name 'value-contract5
   `(let ([ctc (unconstrained-domain-> number?)])
      (,test
       #:test-case-name 'value-contract5
       ctc value-contract (contract ctc (λ (x) 3) 'pos 'neg))))
  (contract-eval
   #:test-case-name 'value-contract6
   `(let ([ctc (case-> (-> number? number? number?) (-> number? number?))])
      (,test
       #:test-case-name 'value-contract6
       ctc value-contract (contract ctc (case-lambda [(x) 3] [(x y) (+ x y)]) 'pos 'neg))))
  
  (contract-eval
   #:test-case-name 'value-contract7
   `(let ([ctc (box/c number?)])
      (,test
       #:test-case-name 'value-contract7
       ctc value-contract (contract ctc (box 3) 'pos 'neg))))
  (contract-eval
   #:test-case-name 'value-contract8
   `(let ([ctc (hash/c number? number?)])
      (,test
       #:test-case-name 'value-contract8
       ctc value-contract (contract ctc (make-hash) 'pos 'neg))))
  (contract-eval
   #:test-case-name 'value-contract9
   `(let ([ctc (vectorof number?)])
      (,test
       #:test-case-name 'value-contract9
       ctc value-contract (contract ctc (vector 1 2 3) 'pos 'neg))))
  (contract-eval
   #:test-case-name 'value-contract10
   `(let ([ctc (vector/c number? number?)])
      (,test
       #:test-case-name 'value-contract10
       ctc value-contract (contract ctc (vector 4 5) 'pos 'neg))))
  
  (contract-eval
   #:test-case-name 'value-contract11
   `(let ([ctc (object-contract)])
      (,test
       #:test-case-name 'value-contract11
       ctc value-contract (contract ctc (new object%) 'pos 'neg))))
  
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
   'value-blame
   '(let ()
      (define f
        (contract (-> integer? integer?) (λ (x) x) 'pos 'neg))
      ;; opt/c version doesn't yet have blame, so 
      ;; we require only that when there is blame, that the blame is right.
      (or (and (has-blame? f)
               (blame-positive (value-blame f)))
          'pos))
   'pos))